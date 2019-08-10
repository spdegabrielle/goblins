#lang racket/base

(require "core.rkt"
         "message.rkt"
         racket/async-channel
         racket/match
         racket/exn
         racket/contract)

(struct cmd-external-spawn (actor-handler return-ch))
(struct cmd-<- (to-refr kws kw-args args))
(struct cmd-<-p (to-refr kws kw-args args return-ch))
(struct cmd-call (to-refr kws kw-args args return-ch))
(struct cmd-send-message (msg))
(struct cmd-halt ())

(define (make-vat [actormap (make-whactormap)])
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
                            (display ";;;; Error when attempting to run hive main loop:"
                                     (current-error-port))
                            (display (exn->string err)
                                     (current-error-port))
                            (set! running? #f))])
           (define schedule-local-messages
             (match-lambda
               ['() (void)]
               [(list msg rest ...)
                ;; Mildly more efficiently do this in reverse order
                (schedule-local-messages rest)
                (async-channel-put vat-channel (cmd-send-message msg))]))
           ;; Big ol' TODO on this one
           (define schedule-remote-messages
             (match-lambda
               ['() (void)]
               [(list msg rest ...)
                (schedule-remote-messages rest)
                #;(async-channel-put vat-channel (cmd-send-message msg))
                'TODO]))

           (let lp ()
             (match (async-channel-get vat-channel)
               ;; This is the actual thing this loop spends the most
               ;; time on, so it needs to go first
               [(cmd-send-message msg)
                (define-values (call-result resolve-result _val
                                            transactormap
                                            to-local to-remote)
                  (parameterize ([being-called-by-vat-actor #t])
                    (actormap-turn-message actormap msg
                                           ;; TODO: Come on, we need to do
                                           ;; proper logging
                                           #:display-errors? #t)))
                (transactormap-merge! transactormap)
                (schedule-local-messages to-local)
                (schedule-remote-messages to-remote)
                (lp)]
               [(cmd-external-spawn actor-handler return-ch)
                (define refr
                  (actormap-spawn! actormap actor-handler))
                (channel-put return-ch refr)
                (lp)]
               [(cmd-<- to-refr kws kw-args args)
                (define-values (returned-val transactormap to-local to-remote)
                  (parameterize ([being-called-by-vat-actor #t])
                    (keyword-apply actormap-turn kws kw-args
                                   actormap to-refr args)))
                (transactormap-merge! transactormap)
                (schedule-local-messages to-local)
                (schedule-remote-messages to-remote)
                (lp)]
               ;; This one is trickiest because we also want to
               ;; propagate any errors.
               [(cmd-call to-refr kws kw-args args return-ch)
                (with-handlers ([any/c
                                 (lambda (err)
                                   (channel-put return-ch
                                                (vector 'fail err)))])
                  (define-values (returned-val transactormap to-local to-remote)
                    (parameterize ([being-called-by-vat-actor #t])
                      (keyword-apply actormap-turn kws kw-args
                                     actormap to-refr args)))
                  (transactormap-merge! transactormap)
                  (schedule-local-messages to-local)
                  (schedule-remote-messages to-remote)
                  (channel-put return-ch (vector 'success returned-val)))
                (lp)]
               [(cmd-halt)
                (void)]))))

       ;; Boot it up!
       (dynamic-wind
         (lambda ()
           (set! running? #t))
         do-main-loop
         (lambda ()
           (set! running? #f))))))

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
    (sync/enable-break return-ch))

  (define _<-
    (make-keyword-procedure
     (λ (kws kw-args to-refr . args)
       (async-channel-put vat-channel
                          (cmd-<- to-refr kws kw-args args))
       (void))))

  (define _call
    (make-keyword-procedure
     (λ (kws kw-args to-refr . args)
       (forbid-internal-actor-call)
       (define return-ch
         (make-channel))
       (async-channel-put vat-channel
                          (cmd-call to-refr kws kw-args args return-ch))
       (match (sync/enable-break return-ch)
         [(vector 'success val)
          val]
         [(vector 'fail err)
          (raise err)]))))

  (define (_halt)
    (async-channel-put vat-channel (cmd-halt)))

  (define vat-dispatcher
    (make-keyword-procedure
     (λ (kws kw-args method-name . args)
       (define method
         (match method-name
           ['spawn _spawn]
           ['<- _<-]
           ['call _call]
           ['halt _halt]
           ['is-running? is-running?]))
       (keyword-apply method kws kw-args args))))

  ;; boot the main loop
  (main-loop)

  ;; return the dispatcher
  (procedure-rename vat-dispatcher
                    'vat-dispatcher))

(module+ test
  (require rackunit)
  (define a-vat (make-vat))
  (define friendo (a-vat 'spawn (lambda (bcom) 'hello)))
  (test-equal?
   "vat 'call method"
   (a-vat 'call friendo)
   'hello)

  (define (make-ctr [n 0])
    (lambda (bcom)
      (bcom (make-ctr (add1 n))
            n)))
  (define a-ctr
    (a-vat 'spawn (make-ctr)))
  (check-equal? (a-vat 'call a-ctr) 0)
  (check-equal? (a-vat 'call a-ctr) 1)
  (check-equal? (a-vat 'call a-ctr) 2)
  (check-equal? (a-vat 'call a-ctr) 3)
  (a-vat '<- a-ctr)
  ;; race condition, but I mean, we're in trouble if that's failing :P
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 5)

  (define pokes-ctr
    (a-vat 'spawn (lambda _ (<- a-ctr))))
  (check-equal? (a-vat 'call a-ctr) 6)
  (a-vat 'call pokes-ctr)
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 8)
  (a-vat '<- pokes-ctr)
  (sleep 0.05)
  (check-equal? (a-vat 'call a-ctr) 10))
