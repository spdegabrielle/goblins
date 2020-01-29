#lang racket/base

(provide make-vat)

(require "core.rkt"
         (submod "core.rkt" for-vats)
         "message.rkt"
         racket/async-channel
         racket/match
         racket/exn
         racket/contract
         [only-in racket/promise delay delay/sync force]

         crypto
         crypto/private/common/base256)

(struct cmd-external-spawn (kws kw-args constructor args return-ch))
(struct cmd-send-message (msg))
(struct cmd-call (to-refr kws kw-args args return-ch))
(struct cmd-handle-message (msg))
(struct cmd-halt ())

(define eddsa-impl
  (delay/sync (get-pk 'eddsa (crypto-factories))))

(define (make-eddsa-private-key)
  (generate-private-key (force eddsa-impl) '((curve ed25519))))

;; But what does the machine *do*?
;; Does it need an event loop?
;; Does it talk to the "external world"?
;; It seems obvious that yes, I guess that's the main thing it does,
;; is set up connections to the outside world and route outside
;; connections

;; TODO: If we parameterize this, *when* do we set up the ability
;;   for the machine to be able to speak to the outside world?
;;   especially if this happens through Tor or etc.
;; TODO: Does this need to move into core.rkt?

(define (boot-machine)
  'TODO)

(define current-machine
  (make-parameter (boot-machine)))

(define (register-vat-with-current-machine vat-key vat-dispatcher)
  'TODO)


#;(define (make-machine
         ;; TODO: rename to #:sign/decrypt-key ?
         #:private-key [private-key (delay (make-eddsa-private-key))])
  (define public-key
    (delay
      (pk-key->public-only-key (force private-key))))
  (define public-key-as-bytes
    (delay
      (match (pk-key->datum (force public-key) 'rkt-public)
        [(list 'eddsa public ed25519 public-key-bytes)
         public-key-bytes])))
  )


;; TODO: Maybe restore #:actormap?
;;   But what to do about the vat-connector in that case?
(define (make-vat #:private-key
                  ;; TODO: rename to #:sign/decrypt-key ?
                  [private-key (delay (make-eddsa-private-key))])
  (define public-key
    (delay
      (pk-key->public-only-key (force private-key))))
  (define public-key-as-bytes
    (delay
      (match (pk-key->datum (force public-key) 'rkt-public)
        [(list 'eddsa public ed25519 public-key-bytes)
         public-key-bytes])))

  ;; Weak hashes don't seem to "relinquish" its memory, unfortunately.
  ;; Every now and then we stop and copy over the registry
  ;; to a new registry, see 'gc-registry handler below.
  (define vat-channel
    (make-async-channel))

  #;(define vat-custodian
      (make-custodian))
  #;(define address-will-executor
      (make-will-executor))

  (define running? #f)

  (define being-called-by-vat-actor
    (make-parameter #f))

  (define (forbid-internal-actor-call)
    (when (being-called-by-vat-actor)
      ;; That would have never completed!
      (error "Called a blocking vat method from the vat's own actor")))

  ;; The main loop
  ;; =============
  (define (main-loop)
    ;; Maybe parameterize custodian here in the future
    (thread
     (lambda ()
       (define (do-main-loop)
         (with-handlers ([exn:fail?
                          (lambda (err)
                            (display ";;;; Error when attempting to run vat main loop:"
                                     (current-error-port))
                            (display (exn->string err)
                                     (current-error-port))
                            (set! running? #f))])
           (define schedule-local-messages
             (match-lambda
               ['() (void)]
               [(cons msg rest)
                ;; Mildly more efficiently do this in reverse order
                (schedule-local-messages rest)
                (async-channel-put vat-channel (cmd-handle-message msg))]))

           (define schedule-remote-messages
             (match-lambda
               ['() (void)]
               [(cons msg rest)
                (schedule-remote-messages rest)
                (define to-actor
                  (message-to msg))
                (define vat-connector
                  (live-refr-vat-connector to-actor))
                ;; forward to that vat connector!
                (vat-connector 'handle-message msg)
                (void)]))

           (let lp ()
             (match (async-channel-get vat-channel)
               ;; This is the actual thing this loop spends the most
               ;; time on, so it needs to go first
               [(cmd-handle-message msg)
                (define-values (call-result
                                transactormap
                                to-near to-far)
                  (parameterize ([being-called-by-vat-actor #t])
                    (actormap-turn-message actormap msg
                                           ;; TODO: Come on, we need to do
                                           ;; proper logging
                                           #:display-errors? #t)))
                (transactormap-merge! transactormap)
                (schedule-local-messages to-near)
                (schedule-remote-messages to-far)
                (lp)]
               [(cmd-external-spawn kws kw-args constructor args return-ch)
                (with-handlers ([any/c
                                 (lambda (err)
                                   (channel-put return-ch
                                                (vector 'fail err)))])
                  (define refr
                    (keyword-apply actormap-spawn! kws kw-args actormap constructor args))
                  (channel-put return-ch (vector 'success refr)))
                (lp)]
               [(cmd-send-message msg)
                (async-channel-put vat-channel
                                   (cmd-handle-message msg))
                (lp)]
               [(cmd-call to-refr kws kw-args args return-ch)
                (with-handlers ([any/c
                                 (lambda (err)
                                   (channel-put return-ch
                                                (vector 'fail err)))])
                  (define-values (returned-val transactormap to-near to-far)
                    (parameterize ([being-called-by-vat-actor #t])
                      (keyword-apply actormap-turn kws kw-args
                                     actormap to-refr args)))
                  (transactormap-merge! transactormap)
                  (schedule-local-messages to-near)
                  (schedule-remote-messages to-far)
                  (channel-put return-ch (vector 'success returned-val)))
                (lp)]
               [(cmd-halt)
                ;; TODO: This should be maybe informing the current-machine,
                ;;   once that exists
                #;(send-to-vat-connector (cmd-halt))
                (void)]))))

       ;; Boot it up!
       (dynamic-wind
         (lambda ()
           (set! running? #t))
         do-main-loop
         (lambda ()
           (set! running? #f))))))

  (define (sync-return-ch return-ch)
    (match (sync/enable-break return-ch)
      [(vector 'success val)
       val]
      [(vector 'fail err)
       (raise err)]))

  ;; "Public" methods
  ;; ================
  (define (is-running?)
    running?)

  (define _spawn
    (make-keyword-procedure
     (λ (kws kw-args constructor . args)
       (forbid-internal-actor-call)
       (define return-ch
         (make-channel))
       (async-channel-put vat-channel
                          (cmd-external-spawn kws kw-args constructor
                                              args return-ch))
       (sync-return-ch return-ch))))

  ;; TODO: we need _<-np and _<- ???
  ;;   I guess with external vats, they will provide their own
  ;;   promise, so anyway we need a way to slot in a promise
  (define _<-np
    (make-keyword-procedure
     (λ (kws kw-args to-refr . args)
       (async-channel-put vat-channel
                          (cmd-send-message (message to-refr #f kws kw-args args)))
       (void))))

  (define _call
    (make-keyword-procedure
     (λ (kws kw-args to-refr . args)
       (forbid-internal-actor-call)
       (define return-ch
         (make-channel))
       (async-channel-put vat-channel
                          (cmd-call to-refr kws kw-args args return-ch))
       (sync-return-ch return-ch))))

  (define (_run proc)
    (define ((^_run bcom))
      (proc))
    (_call (_spawn ^_run)))

  (define (_handle-message msg)
    (async-channel-put vat-channel
                       (cmd-send-message msg))
    (void))

  (define (_halt)
    (async-channel-put vat-channel (cmd-halt)))

  (define (_get-vat-id)
    (force public-key-as-bytes))

  ;; be careful!
  (define (_get-vat-private-key)
    (force private-key))

  (define-syntax-rule (define-vat-dispatcher id [method-name method-handler] ...)
    (define id
      (procedure-rename
       (make-keyword-procedure
        (λ (kws kw-args this-method-name . args)
          (define method
            (case this-method-name
              ['method-name method-handler] ...
              [else (error 'vat-dispatcher-error
                           "Unnown method: ~a" this-method-name)]))
          (keyword-apply method kws kw-args args)))
       'id)))

  (define-vat-dispatcher vat-connector
    [handle-message _handle-message]
    [vat-id _get-vat-id])

  (define-vat-dispatcher vat-dispatcher
    [spawn _spawn]
    [<-np _<-np]
    [call _call]
    [vat-id _get-vat-id]
    [vat-private-key _get-vat-private-key]
    [halt _halt]
    [is-running? is-running?]
    [run _run])

  (define actormap
    (make-whactormap #:vat-connector vat-connector))

  ;; boot the main loop
  (main-loop)

  (register-vat-with-current-machine public-key vat-dispatcher)

  ;; return the dispatcher
  vat-dispatcher)

(module+ test
  (require rackunit
           "utils/install-factory.rkt")
  (install-default-factories!)

  (define a-vat (make-vat))
  (define ((^friendo bcom))
    'hello)
  (define friendo (a-vat 'spawn ^friendo))
  (test-equal?
   "vat 'call method"
   (a-vat 'call friendo)
   'hello)

  (define ((^ctr bcom [n 0]))
    (bcom (^ctr bcom (add1 n))
          n))
  (define a-ctr
    (a-vat 'spawn ^ctr))
  (check-equal? (a-vat 'call a-ctr) 0)
  (check-equal? (a-vat 'call a-ctr) 1)
  (check-equal? (a-vat 'call a-ctr) 2)
  (check-equal? (a-vat 'call a-ctr) 3)
  (a-vat '<-np a-ctr)
  ;; race condition, but I mean, we're in trouble if that's failing :P
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 5)

  (define pokes-ctr
    (a-vat 'spawn (lambda _ (lambda _ (<-np a-ctr)))))
  (check-equal? (a-vat 'call a-ctr) 6)
  (a-vat 'call pokes-ctr)
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 8)
  (a-vat '<-np pokes-ctr)
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 10)

  ;; inter-vat communication
  (define b-vat (make-vat))
  (define a-greeter-set-me #f)
  (define a-greeter (a-vat 'spawn (lambda (bcom)
                                    (lambda _
                                      (set! a-greeter-set-me "got it!")))))
  (define b-passoff (b-vat 'spawn (lambda (bcom)
                                    (lambda _
                                      (<-np a-greeter)))))
  (b-vat 'call b-passoff)
  (sleep 0.05)
  (check-equal? a-greeter-set-me "got it!")

  ;; basic inter-vat promise resolution
  (let ([set-this #f])
    (b-vat 'run
           (lambda _
             (on (<- friendo)
                 (lambda (x)
                   (set! set-this (format "I got: ~a" x))))))
    (sleep 0.05)
    (check-equal? set-this "I got: hello"))

  ;; Another promise pipelining test
  (define car-result-here
    #f)
  (define ((^car-factory bcom) color)
    (define ((^car bcom))
      (set! car-result-here (format "The ~a car says: *vroom vroom*!" color)))
    (spawn ^car))
  (define car-factory
    (a-vat 'spawn ^car-factory))
  (a-vat 'run
         (lambda _
           (<-np (<- car-factory 'green))))
  (sleep 0.05)
  (check-equal? car-result-here
                "The green car says: *vroom vroom*!"))
