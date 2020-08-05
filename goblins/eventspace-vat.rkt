#lang racket/base

;; ===================
;; TODO TODO TODO TODO
;; ===================
;;
;; So yeah, we don't really want this in the "Goblins" package because it
;; requires racket/gui and we don't want that to be a requirement.
;; But we could switch to a multi-package setup.

(provide make-eventspace-vat)

(require "core.rkt"
         (submod "core.rkt" for-vats)
         "message.rkt"
         "utils/simple-dispatcher.rkt")


(require racket/gui
         #;crypto
         #;crypto/private/common/base256)

;; TODO: private key stuff?
(define (make-eventspace-vat
         ;; #:private-key
         ;; ;; TODO: rename to #:sign-key ?
         ;; [private-key (delay (make-eddsa-private-key))]
         )
  ;; TODO: #:suspend-to-kill? might be an interesting option, dunno
  (define vat-evtspace
    (make-eventspace))

  (define being-called-by-vat-actor
    (make-parameter #f))

  (define (forbid-internal-actor-call)
    (when (being-called-by-vat-actor)
      ;; That would have never completed!
      (error "Called a blocking vat method from the vat's own actor")))

  (define (call-with-vat-evtspace proc)
    (parameterize ([current-eventspace vat-evtspace])
      (proc)))

  ;; @@: We used to have a more efficient(?) recursive function that
  ;;   never did a reverse, but now I'm writing "for clarity"
  (define (schedule-messages msgs)
    (for ([msg (in-list (reverse msgs))])
      (define to-refr
        (match msg
          [(? message?)
           (message-to msg)]
          [(? listen-request?)
           (listen-request-to msg)]))

      (match to-refr
        [(? local-refr?)
         (define to-vat-connector
           (local-refr-vat-connector to-refr))
         (define this-vat?
           (eq? to-vat-connector vat-connector))
         (if this-vat?
             ;; @@: Here's one thing that's different than the schedule-messages
             ;;   in vat.rkt
             (queue-near-message msg)
             (to-vat-connector 'handle-message msg))]
        [(? remote-refr?)
         (define captp-connector
           (remote-refr-captp-connector to-refr))
         (captp-connector 'handle-message msg)])))

  (define (handle-near-message msg)
    (define-values (call-result transactormap new-messages)
      (parameterize ([being-called-by-vat-actor #t])
        (actormap-turn-message actormap msg
                               ;; TODO: Come on, we need to do
                               ;; proper logging
                               ;; #:display-errors? #t
                               )))
    (transactormap-merge! transactormap)
    (schedule-messages new-messages))

  (define (queue-near-message msg)
    (call-with-vat-evtspace
     (lambda ()
       (queue-callback
        (lambda ()
          (handle-near-message msg))))))

  (define (sync-return-ch return-ch)
    (match (sync/enable-break return-ch)
      [(vector 'success val)
       val]
      [(vector 'fail err)
       (raise err)]))

  ;; "vat dispatcher" and "vat connector" methods
  ;; ============================================

  (define _spawn
    (make-keyword-procedure
     (λ (kws kw-vals constructor . args)
       (forbid-internal-actor-call)
       (define return-ch
         (make-channel))
       
       (call-with-vat-evtspace
        (lambda ()
          (queue-callback
           (lambda ()
             (with-handlers ([any/c
                              (lambda (err)
                                (channel-put return-ch
                                             (vector 'fail err)))])
               (parameterize ([being-called-by-vat-actor #t])
                 (define refr
                   (keyword-apply actormap-spawn! kws kw-vals actormap constructor args))
                 (channel-put return-ch (vector 'success refr))))))))

       (sync-return-ch return-ch))))

  (define _call
    (make-keyword-procedure
     (λ (kws kw-vals to-refr . args)
       (forbid-internal-actor-call)
       (define return-ch
         (make-channel))
       (call-with-vat-evtspace
        (lambda ()
          (queue-callback
           (lambda ()
             (with-handlers ([any/c
                              (lambda (err)
                                (channel-put return-ch
                                             (vector 'fail err)))])
               (define-values (returned-val transactormap new-messages)
                 (parameterize ([being-called-by-vat-actor #t])
                   (keyword-apply actormap-turn kws kw-vals
                                  actormap to-refr args)))
               (transactormap-merge! transactormap)
               (schedule-messages new-messages)
               (channel-put return-ch (vector 'success returned-val)))))))
       (sync-return-ch return-ch))))

  (define (_run proc)
    (define ((^_run _bcom))
      (proc))
    (_call (_spawn ^_run)))

  (define-simple-dispatcher vat-connector
    [handle-message queue-near-message]
    #;[vat-id _get-vat-id])

  (define-simple-dispatcher vat-dispatcher
    #;[vat-id _get-vat-id]
    #;[vat-private-key _get-vat-private-key]
    [spawn _spawn]
    #;[<-np _<-np]
    [call _call]
    #;[halt _halt]
    #;[is-running? is-running?]
    [run _run])

  (define actormap
    (make-whactormap #:vat-connector vat-connector))

  ;; return the dispatcher
  vat-dispatcher)

(module+ test
  (define vat-a
    (make-eventspace-vat))
  
  'TODO)

(module+ try-it-out
  (define gui-vat
    (make-eventspace-vat))

  (define ((^msg-label-setter bcom msg) new-label)
    (send msg set-label new-label))

  (define (setup-gui)
    (define frame
      (new frame%
           [label "Example"]))
    (define msg
      (new message%
           [parent frame]
           [label "No events so far..."]))

    (new button%
         [parent frame]
         [label "Click me"]
         [callback
          (λ (button event)
            (send msg set-label "Button click"))])

    (send frame show #t)

    ;; return a way to set this message manually
    (spawn ^msg-label-setter msg))

  (define label-setter
    (gui-vat 'run setup-gui))

  (gui-vat 'call label-setter "Hello new label!")

  )
