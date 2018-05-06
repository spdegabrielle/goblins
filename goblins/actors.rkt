#lang racket

(require data/queue
         racket/exn
         racket/generic
         racket/promise
         racket/random
         racket/async-channel)

(module+ test
  (require rackunit))

(struct message
  (;; a possible randomly generated identifier for this message.
   ;; Only used across vats?
   ;; @@: Do we even need this?  Maybe the vat is responsible for handling
   ;;   a mapping of these when they move across boundaries?  Hm... nah...
   ;;   that would be expensive because figuring out when to GC ids
   ;;   would be hard.
   id
   ;; Address we're sending this to
   to
   ;; Keywords we'll be applying
   kws
   ;; Values for keywords we'll be applying
   kw-args
   ;; The positional arguments to the procedure we'll be calling
   args
   ;; If we're responding to another message
   in-reply-to
   ;; A request to reply to this actor address
   please-reply-to))

(define-generics address
  [address-id address])

;; TODO: Maybe we merge these with remote-address
(struct local-address
  (id vat-channel)
  #:methods gen:address
  [(define (address-id address)
     (force (local-address-id address)))]
  #:property prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args this . args)
     (keyword-apply <-wait kws kw-args this args))))

(struct remote-address
  (id vat-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

(define (send-message to kws kw-args args
                      #:please-reply-to [please-reply-to #f]
                      #:in-reply-to [in-reply-to #f])
  "Send a message to TO address (through the vat), with BODY.
If PLEASE-REPLY? is true, ask for the recipient to eventually respond
to us."
  (define msg
    (message #f  ;; I guess we'll set! these later if need be?
             to kws kw-args args
             in-reply-to
             please-reply-to))
  (channel-put (send (current-vat) get-vat-channel)
               (vector 'send-message msg))
  msg)

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (send-message to kws kw-args args)
     (void))))

(define <-wait
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (if (self)
         ;; This is being called from an actor, great we can reply to it
         (call-with-composable-continuation
          (lambda (k)
            (abort-current-continuation actor-prompt-tag k to kws kw-args args))
          actor-prompt-tag)
         ;; It's not being called from an actor?
         ;; Well let's make an actor that can reply to it
         ;; FIXME: Propagate errors
         (let* ([ch (make-channel)]
                [tmp-actor
                 (spawn
                  ;; a one-time-use actor...
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (keyword-apply <-wait kws kw-args to args))
                      (make-keyword-procedure
                       (lambda (kws kw-args . args)
                         (channel-put ch (vector kws kw-args args)))))))])
           ;; call the actor once to get it to run
           (<- tmp-actor)
           ;; now retrieve the value
           (match (channel-get ch)
             ([vector kws kw-args args]
              ;; Re-raise values to this continuation
              (keyword-apply values kws kw-args args))))))))

(define (<-block to . body)
  'TODO)

(provide <- <-wait <-block)

(define actor-prompt-tag
  (make-continuation-prompt-tag))

(define current-vat
  (make-parameter #f))

;; Or actor-address?  Anyway maybe this should be a weak box?
(define self
  (make-parameter #f))

(provide current-vat self)

(struct registered-actor
  [handler
   waiting-on-replies
   ;;;; The following two properties are *only* ever managed by the vat's main
   ;;;; thread!
   ;; If we're busy we stuff things to do in our backlog until we can get to them.
   backlog
   ;; Are we busy?  This means we're currently managing a message or have queued
   ;; up some work to do so.
   [busy? #:mutable]])

(define (handle-message actor-reg actor-address msg)
  (match actor-reg
    [(registered-actor handler waiting-on-replies backlog busy?)
     (define (with-message-capture-prompt thunk)
       (call-with-continuation-prompt
        thunk
        actor-prompt-tag
        (lambda (k to kws kw-args args)
          (define msg
            (send-message to kws kw-args args
                          #:please-reply-to actor-address))
          ;; Set waiting-on-replies continuation to be this msg
          (hash-set! waiting-on-replies msg k))))

     (cond
      ;; This message is in reply to another... so handle it!
      [(message-in-reply-to msg) =>
       (lambda (in-reply-to)
         (when (not (hash-has-key? waiting-on-replies in-reply-to))
           (error "Message in reply to object that doesn't exist"
                  msg in-reply-to))
         ;; Pull the waiting continuation out
         (define waiting-kont (hash-ref waiting-on-replies in-reply-to))
         (hash-remove! waiting-on-replies in-reply-to)
         ;; Let's call it...
         (with-message-capture-prompt
          (lambda ()
            (keyword-apply waiting-kont
                           (message-kws msg)
                           (message-kw-args msg)
                           (message-args msg)))))]
      [else
       ;; Set up initial escape and capture prompts, along with error handler
       (with-message-capture-prompt
        (lambda ()
          ;; Why put self parameterization here?  Why not at the top?
          ;; If we put it here, we can ensure that while processing a message
          ;; that we keep self from being gc'ed, but we allow the possibility
          ;; of the message handler being GC'ed from the main root, allowing
          ;; for shutdown when no references are left
          (parameterize ([self actor-address])
            (with-handlers ([exn:fail?
                             (lambda (v)
                               ;; Error handling goes here!
                               (display (exn->string v)))])
              (call-with-values
                  (lambda ()
                    (keyword-apply handler
                                   (message-kws msg)
                                   (message-kw-args msg)
                                   (message-args msg)))
                (make-keyword-procedure
                 (lambda (kws kw-args . args)
                   (when (message-please-reply-to msg)
                     (send-message (message-please-reply-to msg)
                                   kws kw-args args
                                   #:in-reply-to msg)))))))))])
     (send (current-vat) work-finished actor-reg)])
  (void))

(struct available-work
  [registered-actor actor-address message])

;; So we need flexible vats eventually.
;; But do we really need flexible actors? :\
(define vat%
  (class object%
    (super-new)

    ;; This registry doesn't "relinquish" its memory, unfortunately.
    ;; Every now and then we stop and copy over the registry
    ;; to a new registry, see 'gc-registry handler below.
    (define actor-registry
      (make-weak-hasheq))
    (define vat-channel
      (make-channel))
    (define/public (get-vat-channel)
      vat-channel)

    (define vat-custodian
      (make-custodian))

    (define thread-pool-size
      1000)
    ;; Where we put available work 
    (define work-channel
      (make-async-channel))

    (define address-will-executor
      (make-will-executor))

    ;; FIXME: this is really only for debugging, and thus should not be
    ;;   exposed on the default vat.
    (define/public (get-actor-registry)
      actor-registry)

    (define/public (work-finished actor-reg)
      (channel-put vat-channel
                   (vector 'work-finished actor-reg)))

    (define/public (main-loop)
      (parameterize ([current-custodian vat-custodian])
        (thread
         (lambda ()
           ;; executes wills for the actor addresses going out of scope
           (define executor-thread
             (thread
              (lambda ()
                (define steps-till-gc 25000)
                (let loop (;; Whats a good number for this?  I dunno.
                           [countdown-till-gc-registry steps-till-gc])
                  (will-execute address-will-executor)
                  (if (= countdown-till-gc-registry 0)
                      ;; we need to manually clear out the registry every
                      ;; now and then because weak maps are imperfect about
                      ;; freeing up all their information in racket
                      ;; (or so it appears to me from tests...)
                      (begin
                        (channel-put vat-channel 'gc-registry)
                        (loop steps-till-gc))
                      (loop (- countdown-till-gc-registry 1)))))))

           (define (listen-for-work)
             (parameterize ([current-vat this])
               (let lp ()
                 (match (async-channel-get work-channel)
                   [(available-work registered-actor actor-address message)
                    (handle-message registered-actor actor-address message)
                    (lp)]))))

           (define thread-pool
             (for/list ([i (in-range thread-pool-size)])
               (thread listen-for-work)))

           (let lp ()
             (match (channel-get vat-channel)
               ;; Send a message to an actor at a particular address
               [(vector 'send-message msg)
                (define msg-to
                  (message-to msg))
                ;; TODO: Remote vat support goes here
                (when (not (hash-has-key? actor-registry msg-to))
                  (error "No actor with id" msg-to))

                (define actor-reg
                  (hash-ref actor-registry msg-to))
                (if (registered-actor-busy? actor-reg)
                    ;; If the actor is busy, we actually queue this to their
                    ;; backlog...
                    (enqueue! (registered-actor-backlog actor-reg)
                              (available-work actor-reg msg-to msg))
                    ;; Otherwise, work away!
                    (begin
                      ;; set ourselves as busy
                      (set-registered-actor-busy?! actor-reg #t)
                      ;; and off to work we go!
                      (async-channel-put work-channel
                                         (available-work actor-reg msg-to msg))))
                (lp)]
               [(vector 'work-finished actor-reg)
                (define backlog
                  (registered-actor-backlog actor-reg))
                (if (queue-empty? backlog)
                    ;; Nothing left to do?  Let's mark ourselves as idle
                    (set-registered-actor-busy?! actor-reg #f)
                    ;; Otherwise, let's send another task to the workers
                    (async-channel-put work-channel
                                       (dequeue! backlog)))
                (lp)]
               ;; Register an actor as part of this vat
               [(vector 'spawn-actor handler send-actor-address-ch will)
                (define actor-address
                  (local-address #f vat-channel))
                ;; If the user gave us a will to execute, run that
                ;; (when will)
                (will-register address-will-executor
                               actor-address
                               (lambda (v)
                                 (when will
                                   (will))))
                (hash-set! actor-registry
                           actor-address
                           (registered-actor handler
                                             (make-hasheq)
                                             (make-queue)
                                             #f))
                (channel-put send-actor-address-ch
                             actor-address)
                (lp)]
               ;; "Garbage collect" the registry via stop-and-copy
               ['gc-registry
                (define new-registry
                  (make-weak-hasheq))
                (for (([key val] actor-registry))
                  (hash-set! new-registry key val))
                (set! actor-registry new-registry)
                (lp)]
               ['shutdown
                (custodian-shutdown-all vat-custodian)
                (void)])))))
      (void))))

(provide vat%)

(define (spawn handler #:will [will #f])
  (define (spawn-default-vat)
    (define new-vat
      (new vat%))
    (send new-vat main-loop)
    (current-vat new-vat)
    new-vat)
  (define vat
    (or (current-vat)
        (spawn-default-vat)))
  (define get-address-ch
    (make-channel))
  (channel-put (send vat get-vat-channel) (vector 'spawn-actor
                                                  handler get-address-ch
                                                  will))
  (define actor-address
    (channel-get get-address-ch))
  actor-address)

(provide spawn)

(module+ test
  (define put-stuff-in-me
    (box #f))
  (define wait-for-put
    (make-semaphore))
  (define putter
    (spawn
     (lambda (thing)
       (set-box! put-stuff-in-me thing)
       (semaphore-post wait-for-put))))
  ;; TODO: have a time delay on this
  (<- putter "cat")
  (semaphore-wait wait-for-put)
  (test-equal?
   "<- works"
   (unbox put-stuff-in-me)
   "cat")

  (define actor-a
    (spawn
     (lambda (beep [boop 33] #:bop bop)
       (list beep boop bop))))
  (test-equal?
   "Basic <-wait works"
   (<-wait actor-a 1 #:bop 66)
   (list 1 33 66))
  (test-equal?
   "Calling actor-a just as a procedure should behave the same"
   (actor-a 1 #:bop 66)
   (list 1 33 66)))

;;; Classes and objects
(define (spawn-object obj)
  "Spawn class derived object OBJ as an actor"
  (spawn
   (make-keyword-procedure
    (lambda (kws kw-args method . args)
      (keyword-apply dynamic-send
                     kws kw-args obj method args)))))

;;; Shortcut for (spawn-object (new args ...))
(define-syntax-rule (spawn-new args ...)
  (spawn-object
   (new args ...)))

(provide spawn-object spawn-new)

(module+ test
  (define greeter%
    (class object%
      (super-new)
      (define/public (greet name)
        (string-append "Hello, " name "!"))))

  (define groucher%
    (class greeter%
      (super-new)
      (init [annoyed-by "my back"])
      (define am-annoyed-by annoyed-by)
      (define/override (greet name)
        (string-append "Grumble grumble... "
                       (super greet name)
                       "... "
                       am-annoyed-by " is irritating me..."))))

  (let ([ernie
         (spawn-new greeter%)]
        [oscar
         (spawn-new groucher%
                    [annoyed-by "your hair"])])
    (test-equal?
     "Basic actor class-objects work"
     (ernie 'greet "Bert")
     "Hello, Bert!")
    (test-equal?
     "Inherited actor class-objects work"
     (oscar 'greet "Bert")
     "Grumble grumble... Hello, Bert!... your hair is irritating me...")))
