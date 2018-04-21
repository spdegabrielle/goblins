#lang racket

(require racket/exn
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
   ;; The arguments to the procedure we'll be calling
   body
   ;; If we're responding to another message
   in-reply-to
   ;; A request to reply to this actor address
   please-reply-to))

(define-generics address
  [address-id address])

;; TODO: This needs to be callable...
;; TODO: Maybe we merge these with remote-address
;; TODO: This needs a will for when there are no more references
;;   to it where we ask the vat to shut us down
;;   ... or should we use a custodian, actually?
(struct local-address
  (id vat-channel)
  #:methods gen:address
  [(define (address-id address)
     (force (local-address-id address)))])

(struct remote-address
  (id vat-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

(define (send-message to body
                      #:please-reply? [please-reply? #f]
                      #:in-reply-to [in-reply-to #f]
                      )
  "Send a message to TO address (through the vat), with BODY.
If PLEASE-REPLY? is true, ask for the recipient to eventually respond
to us."
  (define msg
    (message #f  ;; I guess we'll set! these later if need be?
             to body
             in-reply-to
             (if please-reply?
                 (self) #f)))
  (channel-put (send (current-vat) get-vat-channel)
               (vector 'send-message msg))
  msg)

(define (<- to . body)
  (send-message to body)
  (void))

(define (<-wait to . body)
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation actor-prompt-tag k to body))
   actor-prompt-tag))

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
  [handler custodian waiting-on-messages])

(define (handle-message actor-reg actor-address msg)
  (match actor-reg
    [(registered-actor handler custodian waiting-on-messages)
     (define (with-message-capture-prompt thunk)
       (call-with-continuation-prompt
        thunk
        actor-prompt-tag
        (lambda (k to body)
          (define msg
            (send-message to body))
          ;; Set waiting-on-messages continuation to be this msg
          (hash-set! waiting-on-messages msg k))))

     (parameterize ([current-custodian custodian])
       (cond
        ;; This message is in reply to another... so handle it!
        [(message-in-reply-to msg) =>
         (lambda (in-reply-to)
           (when (not (hash-has-key? waiting-on-messages in-reply-to))
             (error "Message in reply to object that doesn't exist"
                    msg in-reply-to))
           ;; Pull the waiting continuation out
           (define waiting-kont (hash-ref waiting-on-messages in-reply-to))
           (hash-remove! waiting-on-messages in-reply-to)
           ;; Let's call it...
           (with-message-capture-prompt
            (lambda ()
              (apply waiting-kont (message-body msg)))))]
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
                      (apply handler (message-body msg)))
                  (lambda vals
                    (when (message-please-reply-to msg)
                      (send-message (message-please-reply-to msg)
                                    vals
                                    #:in-reply-to msg))))))))]))])
  (void))

(struct available-work
  [registered-actor actor-address message])

;; So we need flexible vats eventually.
;; But do we really need flexible actors? :\
(define vat%
  (class object%
    (super-new)

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

    (define/public (main-loop)
      (parameterize ([current-custodian vat-custodian])
        (thread
         (lambda ()
           ;; executes wills for the actor addresses going out of scope
           (define executor-thread
             (thread
              (lambda ()
                (let loop ()
                  (will-execute address-will-executor)
                  (loop)))))

           (define (listen-for-work)
             (let lp ()
               (match (async-channel-get work-channel)
                 [(available-work registered-actor actor-address message)
                  (handle-message registered-actor actor-address message)
                  (lp)])))

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
                (async-channel-put work-channel
                                   (available-work actor-reg msg-to msg))
                (lp)]
               ;; Register an actor as part of this vat
               ;; TODO: Perhaps we should be the ones generating the address
               [(vector 'spawn-actor handler send-actor-address-ch)
                (define actor-custodian
                  (make-custodian))
                (define actor-address
                  (local-address #f vat-channel))
                ;; Set this custodian up to shut down this actor's custodian
                ;; once the actor address is gone
                (will-register address-will-executor
                               actor-address
                               (lambda (v)
                                 (display "So long, pal!\n")
                                 (custodian-shutdown-all actor-custodian)))
                (hash-set! actor-registry
                           actor-address
                           (parameterize ([current-custodian actor-custodian])
                             (registered-actor handler
                                               actor-custodian
                                               (make-hasheq))))
                (channel-put send-actor-address-ch
                             actor-address)
                (lp)]
               ['shutdown
                (custodian-shutdown-all vat-custodian)
                (void)])))))
      (void))))

(provide vat%)

(define (spawn handler #:will [will #f])  ; TODO: Add will support
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
  (channel-put (send vat get-vat-channel) (vector 'spawn-actor handler get-address-ch))
  (define actor-address
    (channel-get get-address-ch))
  actor-address)

(provide spawn)
