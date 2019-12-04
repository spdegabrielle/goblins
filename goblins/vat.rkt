#lang racket/base

(provide make-vat)

(require "core.rkt"
         (submod "core.rkt" for-vats)
         "message.rkt"
         "machine.rkt"
         "actor-lib/methods.rkt"
         racket/async-channel
         racket/match
         racket/exn
         racket/contract)

(struct cmd-external-spawn (actor-handler return-ch))
(struct cmd-<- (msg))
(struct cmd-<-p (msg return-ch))
(struct cmd-call (to-refr kws kw-args args return-ch))
(struct cmd-send-message (msg))
(struct cmd-halt ())

;; TODO: Maybe restore #:actormap?
;;   But what to do about the vat-connector in that case?
(define (make-vat)
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
                (async-channel-put vat-channel (cmd-send-message msg))]))

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
               [(cmd-send-message msg)
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
                (schedule-remote-messages to-far)]
               [(cmd-external-spawn actor-handler return-ch)
                (with-handlers ([any/c
                                 (lambda (err)
                                   (channel-put return-ch
                                                (vector 'fail err)))])
                  (define refr
                    (actormap-spawn! actormap actor-handler))
                  (channel-put return-ch (vector 'success refr)))]
               [(cmd-<- msg)
                (async-channel-put vat-channel
                                   (cmd-send-message msg))]
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
                  (channel-put return-ch (vector 'success returned-val)))]
               [(cmd-halt)
                (set! running? #f)
                ;; TODO: This should be maybe informing the current-machine,
                ;;   once that exists
                #;(send-to-vat-connector (cmd-halt))
                (void)])
             (when running? (lp)))))

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

  (define (_spawn actor-handler
                  [debug-name (object-name actor-handler)])
    (forbid-internal-actor-call)
    (define return-ch
      (make-channel))
    (async-channel-put vat-channel
                       (cmd-external-spawn actor-handler return-ch))
    (sync-return-ch return-ch))

  ;; TODO: we need _<- and _<-p ???
  ;;   I guess with external vats, they will provide their own
  ;;   promise, so anyway we need a way to slot in a promise
  (define _<-
    (make-keyword-procedure
     (λ (kws kw-args to-refr . args)
       (async-channel-put vat-channel
                          (cmd-<- (message to-refr #f kws kw-args args)))
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

  (define (_handle-message msg)
    (async-channel-put vat-channel
                       (cmd-<- msg))
    (void))

  (define (_halt)
    (async-channel-put vat-channel (cmd-halt)))

  (define vat-connector
    (methods
     [handle-message _handle-message]))

  (define vat-dispatcher
    (methods
     [spawn _spawn]
     [<- _<-]
     [call _call]
     [halt _halt]
     [is-running? is-running?]))

  (define actormap
    (make-whactormap #:vat-connector vat-connector))

  ;; boot the main loop
  (main-loop)

  #;(register-vat-with-current-machine public-key vat-dispatcher)

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
    (values (bcom (^ctr bcom (add1 n)))
            n))
  (define a-ctr
    (a-vat 'spawn ^ctr))
  (check-equal? (a-vat 'call a-ctr) 0)
  (check-equal? (a-vat 'call a-ctr) 1)
  (check-equal? (a-vat 'call a-ctr) 2)
  (check-equal? (a-vat 'call a-ctr) 3)
  (a-vat '<- a-ctr)
  ;; race condition, but I mean, we're in trouble if that's failing :P
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 5)

  (define pokes-ctr
    (a-vat 'spawn (lambda _ (lambda _ (<- a-ctr)))))
  (check-equal? (a-vat 'call a-ctr) 6)
  (a-vat 'call pokes-ctr)
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 8)
  (a-vat '<- pokes-ctr)
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
                                      (<- a-greeter)))))
  (b-vat 'call b-passoff)
  (sleep 0.05)
  (check-equal? a-greeter-set-me "got it!")

  ;; basic inter-vat promise resolution
  (let ([set-this #f])
    (b-vat 'call
           (b-vat 'spawn
                  (lambda (bcom)
                    (lambda _
                      (on (<-p friendo)
                          (lambda (x)
                            (set! set-this (format "I got: ~a" x))))))))
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
  (a-vat 'call
         (a-vat 'spawn
                (lambda (bcom)
                  (lambda _
                    (<- (<-p car-factory 'green))))))
  (sleep 0.05)
  (check-equal? car-result-here
                "The green car says: *vroom vroom*!"))
