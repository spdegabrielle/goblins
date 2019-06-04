#lang racket/base

(require "message.rkt"
         "ref.rkt"
         racket/match)

(define (fresh-syscaller actor-map)
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
         (hash-ref actor-map to-ref #f))
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
         (actormap-set! actor-map to-ref new-handler))

       return-val)))

  ;; spawn a new actor
  (define (_spawn actor-handler [debug-name #f])
    (spawn actor-map actor-handler debug-name))

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
    (values actor-map to-local to-remote))

  (values this-syscaller get-internals))

(define (turn* actor-map to-ref kws kw-args args)
  (define-values (sys sys-unsealer)
    (fresh-syscaller actor-map))
  (define-values (result-val new-syscaller)
    (keyword-apply sys kws kw-args 'call to-ref args))
  (apply values
         result-val
         (sys-unsealer (new-syscaller 'sealed-internals))))

(define turn
  (make-keyword-procedure
   (lambda (kws kw-args actor-map to-ref . args)
     (turn* actor-map to-ref kws kw-args args))))

(define (turn-message actor-map message)
  (define to (message-to message))
  (unless (near-ref? to)
    (error "Can only perform a turn on a message to local actors"))
  (turn* actor-map to
         (message-kws message)
         (message-kw-vals message)
         (message-args)))

;; NOTE: This isn't weak.  If we want it to gc, we need
;;   the possibility of weak hashmaps.
;;   The way to probably do this is in a two-hashmap layer...
;;;    (TO BE WRITTEN)
(define (new-actor-map)
  '#hasheq())

(define (spawn! actor-map actor-handler [debug-name #f])
  (define actor-ref
    (make-near-ref debug-name))
  (actor-map-set! actor-map actor-ref actor-handler)
  actor-ref)

;; ;; Do we even need a vat structure?  Maybe the actor-map
;; ;; is all there is in this system.
;; (struct vat (actor-map send-deliveries receive-deliveries))

;; (define (fresh-vat)
;;   (vat (make-weak-hasheq) ))


(module+ test
  (require rackunit)
  (define am (new-actor-map))

  (define ((counter n) sys)
    (values n sys (counter (add1 n))))

  ;; can actors update themselves?
  (define-values (ctr-ref am+ctr)
    (spawn am (counter 1)
           'ctr))
  (define-values (turned-val1 am+ctr1 _to-local _to-remote)
    (turn am+ctr ctr-ref))
  (check-eqv? turned-val1 1)
  (define-values (turned-val2 next-am2 _to-local2 _to-remote2)
    (turn am+ctr1 ctr-ref))
  (check-eqv? turned-val2 2)


  )
