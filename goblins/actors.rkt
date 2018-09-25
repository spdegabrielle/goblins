#lang racket

;; Goblins --- An actor model implementation for Racket
;; Copyright © 2018 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;
;; Dual licensed under LGPLv3 or later (as released by the Free Software
;; Foundation) or Apache v2.
;; 
;; See LICENSE, LICENSE-lgpl.txt and LICENSE-apache.txt for details.

(require data/queue
         net/base64
         racket/exn
         racket/generic
         racket/promise
         racket/random
         racket/async-channel)

(module+ test
  (require rackunit))

(define (make-swiss-num)
  (crypto-random-bytes 56))

(struct message
  (;; Address we're sending this to
   to
   ;; Keywords we'll be applying
   kws
   ;; Values for keywords we'll be applying
   kw-args
   ;; The positional arguments to the procedure we'll be calling
   args
   ;; A request to resolve to this resolver
   please-resolve))

(define-generics address
  [address-id address])

;; TODO: Maybe we merge these with remote-address
(struct local-address
  (id hive)
  #:methods gen:address
  [(define (address-id address)
     (force (local-address-id address)))]
  #:methods gen:custom-write
  [(define (write-proc local-address port mode)
     (display (string-append "#<local-address "
                             (bytes->string/latin-1
                              (base64-encode
                               (address-id local-address) #""))
                             ">")
              port))]
  #:property prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args this . args)
     (require-current-actable)
     (send (current-actable) run-directly this kws kw-args args))))

(struct remote-address
  (id hive-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

(define (send-message to kws kw-args args
                      #:please-resolve [please-resolve #f])
  "Send a message to TO address (through the hive), with BODY.
If PLEASE-REPLY? is true, ask for the recipient to eventually respond
to us."
  (require-current-actable)
  (define msg
    (message to kws kw-args args please-resolve))
  (send (current-actable) send-message msg)
  msg)

(define <-no-promise
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (send-message to kws kw-args args)
     (void))))

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (define-values (new-promise new-resolver)
       (spawn-promise-pair))
     (send-message to kws kw-args args
                   #:please-resolve new-resolver)
     new-promise)))

(define <<-
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
                          (keyword-apply <<- kws kw-args to args))
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

(define listener%
  (class object%
    (super-new)
    (init-field new-promise
                new-resolver
                on-fulfilled
                on-catch
                on-finally)
    (define/public (fulfilled val)
      (if on-fulfilled
          (with-handlers ([exn:fail?
                           (lambda (err)
                             (new-resolver 'broken err))])
            (define result
              (on-fulfilled val))
            (new-resolver 'fulfilled val))
          (new-resolver 'fulfilled #f))  ; or void...?
      (when on-finally
        (on-finally)))
    (define/public (broken err)
      (when on-catch
        (with-handlers ([exn:fail?
                         (lambda (err)
                           ;; if you break on on-catch, then um...
                           ;; I guess that's the new error
                           (new-resolver 'broken err))])
          (on-catch err)
          (new-resolver 'broken err)))
      (new-resolver 'broken err)
      (when on-finally
        (on-finally)))))

;; Or maybe "listen" ?
(define (on promise [on-fulfilled #f]
            #:catch [on-catch #f]
            #:finally [on-finally #f])
  (require-current-actable)
  (define-values (new-promise new-resolver)
    (spawn-promise-pair))
  (define listener
    (spawn-new listener%
               [new-promise new-promise]
               [new-resolver new-resolver]
               [on-fulfilled on-fulfilled]
               [on-catch on-catch]
               [on-finally on-finally]))
  (send (current-actable) listen-to-promise promise listener)
  new-promise)

(provide <- <-no-promise <<- on)

(define actor-prompt-tag
  (make-continuation-prompt-tag))

(define actor-address
  (make-parameter #f))
(define (self)
  (actor-address))

(provide self)

;; This is the parameterized object that allows us to reach into the
;; hive to do things as an actor
(define current-actable
  (make-parameter #f))

(define actable<%>
  (interface ()
    send-message spawn))

(define actable-send-message
  (generic actable<%> send-message))
(define actable-spawn
  (generic actable<%> spawn))

(define (require-current-actable)
  (unless (current-actable)
    (error "Not in actor context.")))

(define (forbid-actable)
  (when (current-actable)
    (error "This hive method is not safe to run inside an actor context.")))

;; So we need flexible hives eventually.
;; But do we really need flexible actors? :\
(define hive%
  (class object%
    (super-new)

    ;; This registry doesn't "relinquish" its memory, unfortunately.
    ;; Every now and then we stop and copy over the registry
    ;; to a new registry, see 'gc-registry handler below.
    (define actor-registry
      (make-weak-hasheq))
    (define hive-channel
      (make-channel))

    (define hive-custodian
      (make-custodian))

    (define address-will-executor
      (make-will-executor))

    (define running? #f)

    (define (do-spawn actor [will #f])
      (define actor-address
        (local-address (delay (make-swiss-num)) this))
      ;; If the user gave us a will to execute, run that
      ;; (when will)
      (will-register address-will-executor
                     actor-address
                     (lambda (v)
                       (when will
                         (will))))
      (hash-set! actor-registry actor-address actor)
      actor-address)

    (define this-hive this)

    ;; Simplest version of the actable% class
    (define actable%
      (class* object% (actable<%>)
        (super-new)
        ;; TODO: should we refactor this so that it "queues up"
        ;;   messages to be sent...?
        ;; TODO: maybe this should actually be <-
        (define/public (send-message to kws kw-args args
                                     #:please-resolve
                                     [please-resolve #f])
          ;; TODO: We'll put inter-hive-communication here
          (define msg
            (message to kws kw-args args please-resolve))
          (channel-put hive-channel (vector 'send-message msg))
          msg)
        (define/public (spawn actor [will #f])
          (do-spawn actor will))
        (define/public (listen-to-promise promise listener)
          ;; TODO: Add support for remote hives here
          (define promise-object
            (hash-ref actor-registry promise))
          (unless (promise? promise-object)
            (error "Not a promise so we can't listen to it"))
          (promise-add-listener! promise listener)
          (void))
        (define/public (run-directly actor-ref kws kw-args args)
          (if (and (local-address? actor-ref)
                   (eq? (local-address-hive actor-ref) this-hive))
              (let ([actor (hash-ref actor-registry actor-ref)])
                (displayln (format "actor: ~a" actor))
                (keyword-apply (actor-handler actor)
                               kws kw-args args))
              (error "Can't directly run an actor not on the same hive")))))

    ;; FIXME: this is really only for debugging, and thus should not be
    ;;   exposed on the default hive.
    #;(define/public (get-actor-registry)
      actor-registry)

    (define/public (is-running?)
      running?)

    (define (handle-message actor actor-address msg)
      ;; Set up initial escape and capture prompts, along with error handler
      (call-with-continuation-prompt
       (lambda ()
         ;; Why put self parameterization here?  Why not at the top?
         ;; If we put it here, we can ensure that while processing a message
         ;; that we keep self from being gc'ed, but we allow the possibility
         ;; of the actor being GC'ed from the main root, allowing
         ;; for shutdown when no references are left
         (parameterize ([actor-address actor-address]
                        [current-actable (new actable%)])
           (define please-resolve
             (message-please-resolve msg))
           (with-handlers ([exn:fail?
                            (lambda (v)
                              ;; Error handling goes here!
                              (display (exn->string v))
                              (when please-resolve
                                ;; Or should we use <-no-promise?
                                (please-resolve 'broken v))
                              (void))])
             (call-with-values
              (lambda ()
                (keyword-apply (actor-handler actor)
                               (message-kws msg)
                               (message-kw-args msg)
                               (message-args msg)))
              (make-keyword-procedure
               (lambda (kws kw-args . args)
                 (when please-resolve
                   (please-resolve 'fulfilled args))))))))
       actor-prompt-tag
       (lambda (k to kws kw-args args)
         #;(define msg
             (send-message to kws kw-args args
                           #:please-reply-to actor-address))
         ;; Set waiting-on-replies continuation to be this msg
         #;(hash-set! waiting-on-replies msg k)
         ;; FIXME: Redo this
         'TODO)))

    ;; Bootstrapping methods
    ;; =====================
    (define/public (spawn actor #:will [will #f])
      (forbid-actable)
      (define return-ch
        (make-channel))
      (channel-put hive-channel
                   (vector 'external-spawn actor will return-ch))
      (channel-get return-ch))

    ;; The main loop, at last
    ;; ======================
    (define (main-loop)
      (parameterize ([current-custodian hive-custodian])
        (thread
         (lambda ()
           (with-handlers ([exn:fail?
                            (set! running? #f)])

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
                          (channel-put hive-channel 'gc-registry)
                          (loop steps-till-gc))
                        (loop (- countdown-till-gc-registry 1)))))))

             #;(define (listen-for-work)
                 (parameterize ([current-hive this])
                   (let lp ()
                     (match (async-channel-get work-channel)
                       [(available-work registered-actor actor-address message)
                        (handle-message registered-actor actor-address message)
                        (lp)]))))

             (let lp ()
               (match (channel-get hive-channel)
                 ;; Send a message to an actor at a particular address
                 [(vector 'send-message msg)
                  (define msg-to
                    (message-to msg))
                  ;; TODO: Remote hive support goes here
                  (when (not (hash-has-key? actor-registry msg-to))
                    (error "No actor with id" msg-to))

                  (define actor
                    (hash-ref actor-registry msg-to))

                  ;; Do a "turn"
                  (handle-message actor msg-to msg)
                  
                  (lp)]
                 ;; spawn initialized through some external process.
                 ;; Really, since external hives can't do this, only
                 ;; through the Hive itself.
                 [(vector 'external-spawn actor will return-ch)
                  (define actor-id
                    ;; TODO: I don't think we need this, but maybe we do?
                    ;; (parameterize ([current-actable (new actable%)]))
                    (do-spawn actor will))
                  (channel-put return-ch actor-id)
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
                  (custodian-shutdown-all hive-custodian)
                  (void)]))
             (set! running? #f)))))
      (void))
    ;; TODO: Maybe eventually allow the user to "restart" the main loop?
    (main-loop)))

(provide hive%)

;; Defines core actor behaviors.  Some of E's "miranda" methods
;; go here.
(define-generics actor
  ;; (-> actor? string?)
  (actor-printed-repr actor)
  ;; (-> actor? procedure?)
  (actor-handler actor)
  #:fallbacks
  [(define (actor-printed-repr actor)
     (~a actor))]
  #:fast-defaults
  ([procedure?
    (define (actor-handler actor)
      actor)]
   [object?
    (define (actor-handler actor)
      (object-actor-handler actor))]))

(define (object-actor-handler obj)
  (make-keyword-procedure
   (lambda (kws kw-args method . args)
     (keyword-apply dynamic-send
                    kws kw-args obj method args))))

(define (spawn actor #:will [will #f])
  (require-current-actable)
  (define actor-address
    (send (current-actable) spawn-actor actor will))
  actor-address)

(provide spawn)

(module+ test
  (define hive
    (new hive%))
  (define put-stuff-in-me
    (box #f))
  (define wait-for-put
    (make-semaphore))
  (define putter
    (send hive spawn
          (lambda (thing)
            (set-box! put-stuff-in-me thing)
            (semaphore-post wait-for-put))))
  ;; TODO: have a time delay on this
  #;(<- putter "cat")
  #;(semaphore-wait wait-for-put)
  #;(test-equal?
   "<- works"
   (unbox put-stuff-in-me)
   "cat")

  ;; (define actor-a
  ;;   (spawn
  ;;    (lambda (beep [boop 33] #:bop bop)
  ;;      (list beep boop bop))))
  ;; (test-equal?
  ;;  "Basic <<- works"
  ;;  (<<- actor-a 1 #:bop 66)
  ;;  (list 1 33 66))
  #;(test-equal?
   "Calling actor-a just as a procedure should behave the same"
   (actor-a 1 #:bop 66)
   (list 1 33 66))
  )

;;; Classes and objects
;;; Shortcut for (spawn-object (new args ...))
(define-syntax-rule (spawn-new args ...)
  (spawn
   (new args ...)))

(provide spawn-new)

#;(module+ test
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
     (<<- ernie 'greet "Bert")
     "Hello, Bert!")
    (test-equal?
     "Inherited actor class-objects work"
     (<<- oscar 'greet "Bert")
     "Grumble grumble... Hello, Bert!... your hair is irritating me...")))

;; TODO: I really ought to separate out this into modules, this is
;; getting unwieldy
(define (spawn-promise-pair)
  (define promise
    (make-promise))
  (define promise-actor
    (spawn promise))
  (define resolver-actor
    (spawn
     (match-lambda*
       [(list 'fulfilled val)
        (fulfill-promise! promise val)]
       [(list 'broken err)
        (break-promise! promise err)]
       [_ (error "Unsupported method")])))
  (values promise-actor resolver-actor))

(struct promise ([state #:mutable]
                 [args-or-error #:mutable]
                 [listeners #:mutable])
  #:methods gen:actor
  [(define (actor-handler actor)
     (make-keyword-procedure
      (lambda (kws kw-args . args)
        (on (self)
            (lambda (val)
              (keyword-apply <-no-promise kws kw-args val args))))))])

(define (make-promise)
  (promise 'waiting #f '()))

(define (promise-maybe-run-listeners! promise)
  (match (promise-state promise)
    ['fulfilled
     (for ([listener (promise-listeners promise)])
       (apply <- listener 'fulfilled
              (promise-args-or-error promise)))
     (set-promise-listeners! promise '())
     (void)]
    ['broken
     (for ([listener (promise-listeners promise)])
       (<- listener 'broken
           (promise-args-or-error promise)))
     (set-promise-listeners! promise '())
     (void)]
    ['waiting (void)]))

(define (promise-add-listener! promise listener)
  (set-promise-listeners!
   (cons listener (promise-listeners promise)))
  (promise-maybe-run-listeners!))

(define (promise-settled? promise)
  (not (eq? (promise-state promise) 'waiting)))

(define (promise-error-if-settled promise)
  (when (promise-settled? promise)
    (error "Promise already settled")))

(define (promise-state-changer state)
  (lambda (promise err-or-args)
    (promise-error-if-settled promise)
    (set-promise-state! promise state)
    (set-promise-args-or-error! promise err-or-args)
    (promise-maybe-run-listeners! promise)
    (void)))
(define break-promise!
  (promise-state-changer 'broken))
(define fulfill-promise!
  (promise-state-changer 'fulfilled))

