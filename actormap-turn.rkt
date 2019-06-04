#lang racket/base

(provide actormap-turn
         actormap-turn-commit!
         actormap-turn-poke
         actormap-turn-message

         new-actormap
         spawn!)

(require "message.rkt"
         "ref.rkt"
         "actormap.rkt"
         "hash-contracts.rkt"
         racket/match)

(define (fresh-syscaller prev-actormap)
  (define actormap
    (make-transactormap prev-actormap))
  (define to-local '())
  (define to-remote '())
  (define this-syscaller
    (make-keyword-procedure
     (lambda (kws kw-args method-id . args)
       (define method
         (match method-id
           ['call call]
           ['spawn _spawn]
           ['<- <-]
           [_ (error "invalid syscaller method")]))
       (keyword-apply method kws kw-args args))))

  ;; call actor's handler
  (define call
    (make-keyword-procedure
     (lambda (kws kw-args to-ref . args)
       (define actor-handler
         (transactormap-ref actormap to-ref #f))
       (unless actor-handler
         (error "Can't send message; no actor with this id"))
       (define-values (return-val new-handler)
         (call-with-values
          (lambda ()
            (keyword-apply actor-handler kws kw-args
                           this-syscaller
                           args))
          (case-lambda
            [(return-val)
             (values return-val #f)]
            [(return-val new-handler)
             (values return-val new-handler)])))

       ;; if a new handler for this actor was specified,
       ;; let's replace it
       (when new-handler
         (transactormap-set! actormap to-ref new-handler))

       return-val)))

  ;; spawn a new actor
  (define (_spawn actor-handler [debug-name #f])
    (spawn! actormap actor-handler debug-name))

  (define <-
    (make-keyword-procedure
     (lambda (kws kw-args actor-ref . args)
       (define new-message
         (message actor-ref kws kw-args args))
       (match actor-ref
         [(? near-ref?)
          (set! to-local (cons new-message to-local))]
         [(? far-ref?)
          (set! to-remote (cons new-message to-remote))]))))

  (define (get-internals)
    (list actormap to-local to-remote))

  (values this-syscaller get-internals))

(define (actormap-turn* actormap to-ref kws kw-args args)
  (define-values (sys get-sys-internals)
    (fresh-syscaller actormap))
  (define result-val
    (keyword-apply sys kws kw-args 'call to-ref args))
  (apply values result-val
         (get-sys-internals)))

(define actormap-turn
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (actormap-turn* actormap to-ref kws kw-args args))))

;; Note that this does nothing with the messages.
(define actormap-turn-commit!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     (transactormap-merge! transactormap)
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-turn-poke
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     returned-val)))

(define (actormap-turn-message actormap message)
  (define to (message-to message))
  (unless (near-ref? to)
    (error "Can only perform a turn on a message to local actors"))
  (actormap-turn* actormap to
         (message-kws message)
         (message-kw-vals message)
         (message-args)))

(define new-actormap
  make-weak-hasheq)

(define (spawn! actormap actor-handler [debug-name #f])
  (define actor-ref
    (make-near-ref debug-name))
  (define map-set!
    (match actormap
      [(? weak-hasheq/c)
       hash-set!]
      [(? transactormap?)
       transactormap-set!]))
  (map-set! actormap actor-ref actor-handler)
  actor-ref)

;; ;; Do we even need a vat structure?  Maybe the actormap
;; ;; is all there is in this system.
;; (struct vat (actormap send-deliveries receive-deliveries))

;; (define (fresh-vat)
;;   (vat (make-weak-hasheq) ))


(module+ test
  (require rackunit)
  (define am (new-actormap))

  (define ((counter n) sys)
    (values n (counter (add1 n))))

  ;; can actors update themselves?
  (define ctr-ref
    (spawn! am (counter 1)
            'ctr))
  (define-values (turned-val1 am+ctr1 _to-local _to-remote)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1 1)
  (define-values (turned-val2 am+ctr2 _to-local2 _to-remote2)
    (actormap-turn am+ctr1 ctr-ref))
  (check-eqv? turned-val2 2)

  ;; transaction shouldn't be applied yet
  (define-values (turned-val1-again
                  am+ctr1-again
                  _to-local-again _to-remote-again)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1-again 1)

  ;; but now it should be
  (transactormap-merge! am+ctr2)
  (define-values (turned-val3 am+ctr3 _to-local3 _to-remote3)
    ;; observe that we're turning using the "original"
    ;; actormap though!  It should have committed.
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val3 3)

  (define (friend-spawner sys friend-name)
    (define ((a-friend [called-times 0]) sys)
      (define new-called-times
        (add1 called-times))
      (values (format "Hello!  My name is ~a and I've been called ~a times!"
                      friend-name new-called-times)
              (a-friend new-called-times)))
    (sys 'spawn (a-friend) 'friend))
  (define fr-spwn (spawn! am friend-spawner))
  (define joe (actormap-turn-commit! am fr-spwn 'joe))
  (check-equal?
   (actormap-turn-poke am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-turn-commit! am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-turn-commit! am joe)
   "Hello!  My name is joe and I've been called 2 times!"))
