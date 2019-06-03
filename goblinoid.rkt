#lang racket/base

(require "seal-unseal.rkt"
         racket/match)

(struct message (to kws kw-vals args))

(struct local-actor-id (debug-name)
  #:transparent
  #:constructor-name make-local-actor-id)

(struct remote-actor-id (remote-vat-id)
  #:transparent
  #:constructor-name make-remote-actor-id)

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
       (lambda (kws kw-args to-id . args)
         (define actor-handler
           (hash-ref actor-map to-id #f))
         (unless actor-handler
           (error "Can't send message; no actor with this id"))
         (define-values (return-val returned-syscaller returned-handler)
           (call-with-values
            (lambda ()
              (keyword-apply actor-handler kws kw-args args))
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
                  (make-syscaller actor-map to-local to-remote)])
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
      (define-values (actor-id new-actor-map)
        (spawn actor-map actor-handler debug-name))
      (values actor-id
              (make-syscaller new-actor-map
                              to-local to-remote)))

    (define <-
      (make-keyword-procedure
       (lambda (kws kw-args actor-id . args)
         (define new-message
           (message actor-id kws kw-args args))
         (match actor-id
           [(? local-actor-id?)
            (make-syscaller actor-map
                            (cons new-message to-local)
                            to-remote)]
           [(? remote-actor-id?)
            (make-syscaller actor-map
                            to-local
                            (cons new-message to-remote))]))))
    this-syscaller)
  (values (make-syscaller actor-map) unseal))

(define (turn* actor-map to-id kws kw-args args)
  (define-values (sys sys-unsealer)
    (fresh-syscaller actor-map))
  (keyword-apply sys kws kw-args 'call to-id args))

(define turn
  (make-keyword-procedure
   (lambda (kws kw-args actor-map to-id . args)
     (turn* actor-map to-id kws kw-args args))))

(define (turn-message actor-map message)
  (define to (message-to message))
  (unless (local-actor-id? to)
    (error "Can only perform a turn on a message to local actors"))
  (turn* actor-map to
         (message-kws message)
         (message-kw-vals message)
         (message-args)))

(define (new-actor-map)
  (make-weak-hasheq))

(define (spawn actor-map actor-handler [debug-name #f])
  (define actor-id
    (make-local-actor-id debug-name))
  (values actor-id
          (hash-set actor-map actor-id actor-handler)))


;; ;; Do we even need a vat structure?  Maybe the actor-map
;; ;; is all there is in this system.
;; (struct vat (actor-map send-deliveries receive-deliveries))

;; (define (fresh-vat)
;;   (vat (make-weak-hasheq) ))


