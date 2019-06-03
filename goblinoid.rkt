#lang racket/base

(require "seal-unseal.rkt"
         racket/match)

;; Do we even need a vat structure?  Maybe the actor-map
;; is all there is in this system.
(struct vat (actor-map send-deliveries receive-deliveries))

(define (fresh-vat)
  (vat (make-weak-hasheq) (make-weak-hasheq)))


(struct local-actor-id ()
  #:constructor-name make-local-actor-id)

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
             ['spawn spawn]
             ['<- <-]
             ['present-self-seal present-self-seal]
             ['syscaller-child? syscaller-child?]
             ['sealed-actor-map sealed-actor-map]
             [_ (error "invalid syscaller method")]))
         (keyword-apply method kws kw-args args))))

    ;; call actor's handler
    (define call
      (make-keyword-procedure
       (lambda (kws kw-args . to-id args)
         (define actor-handler
           (hash-ref actor-map
                     (local-message-to message)
                     #f))
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

         (define syscaller-before-handler
           (or returned-syscaller this-syscaller))

         (define final-syscaller
           (if returned-handler
               (match (unseal (syscaller-before-handler
                               'sealed-internals))
                 [(list actor-map to-local to-remote)
                  (make-syscaller actor-map to-local to-remote)])
               syscaller-before--handler))

         (values return-val final-syscaller))))

    ;; spawn a new actor
    (define (spawn actor-handler)
      (define actor-id
        (make-local-actor-id))
      (define new-actor-map
        (hash-set actor-map actor-id actor-handler))
      (values actor-id
              (make-syscaller new-actor-map
                              to-local to-remote)))
    
    (define (sealed-internals)
      (seal (list actor-map to-local to-remote)))

    this-syscaller

    
    
    #;(match-lambda*
        ;; TODO: Add keyword args
        [(list 'call actor-id args ...)
         ;;(define result)
         ;; (values new-syscaller)
         'TODO]
        [(list 'spawn actor-handler)
         'TODO]
        [(list '<- to-id )]
        [(list 'present-self-seal)
         (seal syscaller)]
        [(list 'sealed-by-me? val)
         (sealed? val)]
        )

    )
  (values (make-syscaller actor-map) unseal))

(define (vat-turn vat message)


  

  (define-values (return-val next-vat result-sys)
    (call-with-values
     (lambda ()
       (keyword-apply )
       ))
    )

  (unless ((syscaller-trademark syscaller) result-sys)
    )
  (call-with-values (lambda ()

                      ))

  )

(define (call-actor vat message syscaller)
  )
