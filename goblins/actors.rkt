#lang racket

;; Goblins --- An actor model implementation for Racket
;; Copyright © 2018 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;
;; Dual licensed under LGPLv3 or later (as released by the Free Software
;; Foundation) or Apache v2.
;; 
;; See LICENSE, LICENSE-lgpl.txt and LICENSE-apache.txt for details.

(provide <- <-np <<- on
         current-hive hive%
         spawn spawn-new)

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
   ;; System method, usually 'resolve but may be another
   ;; system/miranda method
   sys-method
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
     (send-generic (current-actable) actable-run-directly
                   this kws kw-args args))))

(struct remote-address
  (id hive-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

(define (send-message to kws kw-args args
                      #:please-resolve [please-resolve #f]
                      #:sys-method [sys-method 'handle])
  "Send a message to TO address (through the hive), with BODY.
If PLEASE-REPLY? is true, ask for the recipient to eventually respond
to us."
  (define msg
    (message to sys-method kws kw-args args please-resolve))
  (cond
    [(current-actable)
     (send-generic (current-actable) actable-send-message msg)]
    [(current-hive)
     (unless (send (current-hive) is-running?)
       (error "Hive is not running"))
     (send (current-hive) send-message msg)]
    [else
     (error "Can't send message if no current-actable nor current-hive")])
  msg)

;; np stands for "No Promise"
(define <-sys-np
  (make-keyword-procedure
   (lambda (kws kw-args to sys-method . args)
     (send-message to kws kw-args args
                   #:sys-method sys-method)
     (void))))

(define <-np
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (keyword-apply <-sys-np kws kw-args to 'handle args))))

(define <-sys
  (make-keyword-procedure
   (lambda (kws kw-args to sys-method . args)
     (define-values (new-promise new-resolver)
       (spawn-promise-pair))
     (send-message to kws kw-args args
                   #:please-resolve new-resolver
                   #:sys-method sys-method)
     new-promise)))

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (keyword-apply <-sys kws kw-args to 'handle args))))

(define <<-sys
  (make-keyword-procedure
   (lambda (kws kw-args to sys-method . args)
     (define resume-data
       (cond
         [(current-actable)
          (call-with-composable-continuation
           (lambda (k)
             (abort-current-continuation actor-prompt-tag k to sys-method
                                         kws kw-args args))
           actor-prompt-tag)]
         [(current-hive)
          (define return-ch
            (make-channel))
          (unless (send (current-hive) is-running?)
            (error "Hive is not running"))
          (<-np (spawn
                 (lambda ()
                   (with-handlers ([exn:fail?
                                    (lambda (err)
                                      (channel-put return-ch
                                                   (vector 'error err)))])
                     (call-with-values
                      (lambda ()
                        (keyword-apply <<-sys kws kw-args to sys-method args))
                      (lambda args
                        (channel-put return-ch (vector 'resume-values args))))))))
          (channel-get return-ch)]
         [else
          (error "Can't send message if no current-actable nor current-hive")]))
     (match resume-data
       [(vector 'resume-values vals)
        (apply values vals)]
       [(vector 'error err)
        ;; TODO: Do we really just want to re-raise the error this way
        (raise err)]))))

(define <<-
  (make-keyword-procedure
   (lambda (kws kw-args to . args)
     (keyword-apply <<-sys kws kw-args to 'handle args))))

(define listener%
  (class object%
    (super-new)
    (init-field on-fulfilled
                on-catch
                on-finally
                [resolve-me #f])
    (define resolved? #f)
    (define (mark-resolved!)
      (when resolved?
        (error "Listener already resolved"))
      (set! resolved? #t))
    (define (wipe-listeners!)
      (set! on-fulfilled #f)
      (set! on-catch #f)
      (set! on-finally #f))
    (define/public (fulfilled vals)
      (mark-resolved!)
      (if on-fulfilled
          (with-handlers ([exn:fail?
                           (lambda (err)
                             (and resolve-me
                                  (resolve-me 'broken err)))])
            (call-with-values
             (lambda ()
               (apply on-fulfilled vals))
             (lambda result
               (and resolve-me
                    (resolve-me 'fulfilled result)))))
          (and resolve-me (resolve-me 'fulfilled #f)))
      (when on-finally
        (on-finally))
      (wipe-listeners!)
      (void))
    (define/public (broken err)
      (mark-resolved!)
      (when on-catch
        (with-handlers ([exn:fail?
                         (lambda (err)
                           ;; if you break on on-catch, then um...
                           ;; I guess that's the new error
                           (and resolve-me
                                (resolve-me 'broken err)))])
          (on-catch err)
          (and resolve-me
               (resolve-me 'broken err))))
      (when on-finally
        (on-finally))
      (wipe-listeners!)
      (void))))

;; Listen to a promise
(define (on promise [on-fulfilled #f]
            #:catch [on-catch #f]
            #:finally [on-finally #f]
            #:promise? [promise? #f])
  (cond
    [(current-actable)
     (cond
       [promise?
        (define-values (new-promise new-resolver)
          (spawn-promise-pair))
        (define listener
          (spawn-new listener%
                     [resolve-me new-resolver]
                     [on-fulfilled on-fulfilled]
                     [on-catch on-catch]
                     [on-finally on-finally]))
        (send-generic (current-actable) actable-listen-to-promise promise listener)
        new-promise]
       [else
        (define listener
          (spawn-new listener%
                     [on-fulfilled on-fulfilled]
                     [on-catch on-catch]
                     [on-finally on-finally]))
        (send-generic (current-actable) actable-listen-to-promise promise listener)
        (void)])]
    [(current-hive)
     (<<- (spawn
           (lambda ()
             (on promise on-fulfilled
                 #:catch on-catch
                 #:finally on-finally
                 #:promise? promise?))))]
    [else
     (error "Can't listen to a promise if no current-actable nor current-hive")]))

(define actor-prompt-tag
  (make-continuation-prompt-tag))

;; This is the parameterized object that allows us to reach into the
;; hive to do things as an actor
(define current-actable
  (make-parameter #f))

(define actable<%>
  (interface ()
    send-message spawn listen-to-promise run-directly))

(define actable-send-message
  (generic actable<%> send-message))
(define actable-spawn
  (generic actable<%> spawn))
(define actable-listen-to-promise
  (generic actable<%> listen-to-promise))
(define actable-run-directly
  (generic actable<%> run-directly))

(define (require-current-actable)
  (unless (current-actable)
    (error "Not in actor context.")))

(define (forbid-actable)
  (when (current-actable)
    (error "This hive method is not safe to run inside an actor context.")))

(define current-hive
  (make-parameter #f))


;; We need flexible hives eventually
(define hive%
  (class object%
    (super-new)

    ;; This registry doesn't "relinquish" its memory, unfortunately.
    ;; Every now and then we stop and copy over the registry
    ;; to a new registry, see 'gc-registry handler below.
    (define actor-registry
      (make-weak-hasheq))
    (define hive-channel
      (make-async-channel))

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
        (define/public (send-message msg)
          ;; TODO: We'll put inter-hive-communication here
          (async-channel-put hive-channel (vector 'send-message msg))
          (void))
        (define/public (spawn actor [will #f])
          (do-spawn actor will))
        (define/public (listen-to-promise promise listener)
          ;; TODO: Add support for remote hives here
          (define promise-object
            (hash-ref actor-registry promise))
          (unless (promise? promise-object)
            (error "Not a promise so we can't listen to it"))
          (promise-add-listener! promise-object listener)
          (void))
        (define/public (run-directly actor-ref kws kw-args args)
          (if (and (local-address? actor-ref)
                   (eq? (local-address-hive actor-ref) this-hive))
              (let ([actor (hash-ref actor-registry actor-ref)])
                (keyword-apply (actor-handler actor)
                               kws kw-args args))
              (error "Can't directly run an actor not on the same hive")))))

    ;; FIXME: this is really only for debugging, and thus should not be
    ;;   exposed on the default hive.
    #;(define/public (get-actor-registry)
      actor-registry)

    (define/public (is-running?)
      running?)

    (define (with-message-capture-prompt thunk)
      (call-with-continuation-prompt
       (lambda ()
         (current-actable (new actable%))
         (thunk)
         (current-actable #f))
       actor-prompt-tag
       (lambda (k to sys-method kws kw-args args)
         (current-actable (new actable%))
         (on (keyword-apply <-sys kws kw-args to sys-method args)
             ;; resume continuation
             (lambda vals
               (async-channel-put hive-channel
                                  (vector 'resume-kont k
                                          (vector 'resume-values vals))))
             #:catch
             (lambda (err)
               (async-channel-put hive-channel
                                  (vector 'resume-kont k
                                          (vector 'error err)))))
         (current-actable #f))))

    (define (handle-message actor actor-address msg)
      ;; Set up initial escape and capture prompts, along with error handler
      (with-message-capture-prompt
       (lambda ()
         ;; TODO: Uh, did we break this by removing self?  See comment
         ;;   below
         ;; Why put self parameterization here?  Why not at the top?
         ;; If we put it here, we can ensure that while processing a message
         ;; that we keep self from being gc'ed, but we allow the possibility
         ;; of the actor being GC'ed from the main root, allowing
         ;; for shutdown when no references are left

         ;; We set rather than parameterize current-actable so we don't
         ;; accidentally build up a bunch of dynamic values on the stack
         (parameterize ([current-actable (new actable%)])
           (define please-resolve
             (message-please-resolve msg))
           (with-handlers ([exn:fail?
                            (lambda (v)
                              ;; Error handling goes here!
                              (display (exn->string v)
                                       (current-error-port))
                              (when please-resolve
                                ;; Or should we use <-np?
                                (please-resolve 'broken v))
                              (void))])
             (match (message-sys-method msg)
               ['handle
                (cond
                  ;; Note that this could possibly be simplified by having
                  ;; actor-handler get access to the entire msg object.  That
                  ;; would allow other actors to choose when to resolve the
                  ;; "please-resolve" message... though there is some risk that
                  ;; they might never do so.
                  [(promise? actor)
                   (on (weak-box-value (promise-this-address actor))
                       (lambda (val)
                         (call-with-values
                          (lambda ()
                            (keyword-apply <<-
                                           (message-kws msg)
                                           (message-kw-args msg)
                                           val
                                           (message-args msg)))
                          (lambda args
                            (when please-resolve
                              (please-resolve 'fulfilled args))))))]
                  [else
                   (call-with-values
                    (lambda ()
                      (keyword-apply (actor-handler actor)
                                     (message-kws msg)
                                     (message-kw-args msg)
                                     (message-args msg)))
                    (make-keyword-procedure
                     (lambda (kws kw-args . args)
                       (when please-resolve
                         (please-resolve 'fulfilled args)))))])]
               [other-method
                (raise-user-error "Invalid actor system method"
                                  other-method)])))
         (current-actable #f))))

    ;; Bootstrapping methods
    ;; =====================
    (define/public (spawn actor #:will [will #f])
      (forbid-actable)
      (define return-ch
        (make-channel))
      (async-channel-put hive-channel
                         (vector 'external-spawn actor will return-ch))
      (channel-get return-ch))

    (define/public (send-message msg)
      (forbid-actable)
      (async-channel-put hive-channel
                         (vector 'send-message msg))
      (void))

    ;; The main loop, at last
    ;; ======================
    (define (main-loop)
      (set! running? #t)
      (parameterize ([current-custodian hive-custodian])
        (thread
         (lambda ()
           (with-handlers ([exn:fail?
                            (lambda (err)
                              (display ";;;; Error when attempting to run hive main loop:"
                                       (current-output-port))
                              (display (exn->string err)
                                       (current-error-port))
                              (set! running? #f))])

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
                          (async-channel-put hive-channel 'gc-registry)
                          (loop steps-till-gc))
                        (loop (- countdown-till-gc-registry 1)))))))
             ;; Why put the handler right outside of the main loop?
             ;; Otherwise, exception handlers start stacking
             (let lp ()
               (match (async-channel-get hive-channel)
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
                 [(vector 'resume-kont k resume-vals)
                  (with-message-capture-prompt
                    (lambda ()
                      (k resume-vals)))
                  (lp)]
                 ;; spawn initialized through some external process.
                 ;; Really, since external hives can't do this, only
                 ;; through the Hive itself.
                 [(vector 'external-spawn actor will return-ch)
                  (define actor-id
                    ;; TODO: I don't think we need this, but maybe we do?
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
  (cond
    [(current-actable)
     (define actor-address
       (send-generic (current-actable) actable-spawn actor will))
     actor-address]
    [(current-hive)
     (define actor-address
       (send (current-hive) spawn actor #:will will))
     actor-address]
    [else
     (define new-hive
       (new hive%))
     (current-hive new-hive)
     (define actor-address
       (send new-hive spawn actor #:will will))
     actor-address]))

(module+ test
  (define put-stuff-in-me
    (box #f))
  (define wait-for-put
    (make-semaphore))

  (define foop
    (spawn (lambda () 'foop)))
  (define boop
    (spawn (lambda (n)
             (for ([i (in-range n)])
               (foop)))))

  (define alice
    (spawn
     (lambda ()
       (displayln 'alice)
       'alicey)))
  (define bob
    (spawn
     (lambda ()
       (displayln 'bob-pre-alice)
       (on (<- alice)
           (lambda (val)
             (displayln (format "got: ~a" val)))
           #:finally
           (lambda ()
             (displayln "TGIF!")))
       (displayln 'bob-post-alice))))
  (define breakable
    (spawn
     (lambda ()
       (error "I broke!")
       'bork)))
  (define uses-breakable
    (spawn
     (lambda ()
       (on (<- breakable)
           (lambda _ (displayln "That worked?  Really???"))
           #:catch
           (lambda (err)
             (displayln (format "got error: ~a" err)))
           #:finally
           (lambda ()
             (displayln "I hate mondays"))))))

  (<- (spawn (lambda ()
               (displayln (format "Got: ~a"
                                  (<<- alice))))))

  (define putter
    (spawn
     (lambda (thing)
       (set-box! put-stuff-in-me thing)
       (semaphore-post wait-for-put))))
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
   "Basic <<- works"
   (<<- actor-a 1 #:bop 66)
   (list 1 33 66))

  (let ([vals
         (call-with-values
          (lambda ()
            (<<- (spawn (lambda ()
                          (values 'a 'b 'c)))))
          (lambda vals vals))])
    (test-equal?
     "Multiple value return works, version 1"
     vals
     (list 'a 'b 'c)))

  (let* ([return-ch (make-channel)]
         [vals
          (begin
            (<- (spawn
                 (lambda ()
                   (channel-put return-ch
                                (call-with-values
                                 (lambda ()
                                   (<<- (spawn (lambda ()
                                                 (values 'a 'b 'c)))))
                                 (lambda vals vals))))))
            (channel-get return-ch))])
    (test-equal?
     "Multiple value return works, version 2"
     vals
     (list 'a 'b 'c))))

;;; Classes and objects
;;; Shortcut for (spawn-object (new args ...))
(define-syntax-rule (spawn-new args ...)
  (spawn
   (new args ...)))

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
     (<<- ernie 'greet "Bert")
     "Hello, Bert!")
    (test-equal?
     "Inherited actor class-objects work"
     (<<- oscar 'greet "Bert")
     "Grumble grumble... Hello, Bert!... your hair is irritating me...")))

;; TODO: I really ought to separate out this into modules, this is
;; getting unwieldy
(define (spawn-promise-pair)
  (define this-promise
    (promise 'waiting #f '() #f))
  (define promise-actor
    (spawn this-promise))
  (set-promise-this-address! this-promise (make-weak-box promise-actor))
  (define resolver-actor
    (spawn
     (match-lambda*
       [(list 'fulfilled val)
        (fulfill-promise! this-promise val)]
       [(list 'broken err)
        (break-promise! this-promise err)]
       [_ (error "Unsupported method")])))
  (values promise-actor resolver-actor))

(struct promise ([state #:mutable]
                 [args-or-error #:mutable]
                 [listeners #:mutable]
                 [this-address #:mutable])
  #:methods gen:actor
  [(define (actor-handler actor)
     (error "Well this never should have been called..."))])

(define (promise-maybe-run-listeners! promise)
  (match (promise-state promise)
    ['fulfilled
     (for ([listener (promise-listeners promise)])
       (<-np listener 'fulfilled
             (promise-args-or-error promise)))
     (set-promise-listeners! promise '())
     (void)]
    ['broken
     (for ([listener (promise-listeners promise)])
       (<-np listener 'broken
             (promise-args-or-error promise)))
     (set-promise-listeners! promise '())
     (void)]
    ['waiting (void)]))

(define (promise-add-listener! promise listener)
  (set-promise-listeners!
   promise
   (cons listener (promise-listeners promise)))
  (promise-maybe-run-listeners! promise))

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

