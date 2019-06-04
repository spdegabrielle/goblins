#lang racket/base

(require "seal-unseal.rkt"
         racket/match)

(struct message (to kws kw-vals args))

(struct near-ref (debug-name)
  #:transparent
  #:constructor-name make-near-ref)

(struct far-ref (remote-vat-id)
  #:transparent
  #:constructor-name make-far-ref)

(define (fresh-syscaller actor-map)
  (define-values (seal unseal sealed?)
    (new-seal))
  (define (make-syscaller actor-map
                          to-local
                          to-remote)
    (define this-syscaller
      (make-keyword-procedure
       (lambda (kws kw-args method-id . args)
         (define method
           (match method-id
             ['call call]
             ['present-self-seal present-self-seal]
             ['syscaller-child? syscaller-child?]
             ['sealed-internals sealed-internals]
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
         (define-values (return-val returned-syscaller returned-handler)
           (call-with-values
            (lambda ()
              (keyword-apply actor-handler kws kw-args
                             this-syscaller
                             args))
            (case-lambda
              [(return-val)
               (values return-val #f #f)]
              [(return-val returned-syscaller)
               (values return-val returned-syscaller #f)]
              [(return-val returned-syscaller returned-handler)
               (values return-val returned-syscaller returned-handler)])))
         ;; uhoh, we got back a syscaller but it doesn't look like
         ;; our child!
         (when (and returned-syscaller
                    (not (this-syscaller 'syscaller-child?
                                         returned-syscaller)))
           (error "Returned syscaller not built from one given to handler"))

         (define syscaller-before-add-handler
           (or returned-syscaller this-syscaller))

         (define final-syscaller
           (if returned-handler
               (match (unseal (syscaller-before-add-handler
                               'sealed-internals))
                 [(list actor-map to-local to-remote)
                  (define new-actor-map
                    (hash-set actor-map to-ref returned-handler))
                  (make-syscaller new-actor-map
                                  to-local to-remote)])
               syscaller-before-add-handler))

         (values return-val final-syscaller))))

    (define (present-self-seal)
      (seal this-syscaller))

    (define (syscaller-child? syscaller)
      (define sc-self-seal
        (syscaller 'present-self-seal))
      (and (sealed? sc-self-seal)  ; we sealed it
           ;; the internals match the object
           ;; aka the trademark validates
           (eq? (unseal sc-self-seal)
                syscaller)))

    ;; present our current values are to someone
    ;; with the unsealer
    (define (sealed-internals)
      (seal (list actor-map to-local to-remote)))

    ;; spawn a new actor
    (define (_spawn actor-handler [debug-name #f])
      (define-values (actor-ref new-actor-map)
        (spawn actor-map actor-handler debug-name))
      (values actor-ref
              (make-syscaller new-actor-map
                              to-local to-remote)))

    (define <-
      (make-keyword-procedure
       (lambda (kws kw-args actor-ref . args)
         (define new-message
           (message actor-ref kws kw-args args))
         (match actor-ref
           [(? near-ref?)
            (make-syscaller actor-map
                            (cons new-message to-local)
                            to-remote)]
           [(? far-ref?)
            (make-syscaller actor-map
                            to-local
                            (cons new-message to-remote))]))))
    this-syscaller)
  (values (make-syscaller actor-map
                          '()
                          '())
          unseal))

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

(define (spawn actor-map actor-handler [debug-name #f])
  (define actor-ref
    (make-near-ref debug-name))
  (values actor-ref
          (hash-set actor-map actor-ref actor-handler)))

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
