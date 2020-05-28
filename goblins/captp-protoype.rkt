#lang racket/base

(require racket/match
         "core.rkt"
         "vat.rkt"
         "actor-lib/methods.rkt"
         syrup)

;;; Messages

(define-syntax-rule (define-recordable-struct struct-id
                      (args ...)
                      struct-marshall-id struct-unmarshall-id)
  (begin (struct struct-id (args ...)
           #:transparent)
         (define struct-marshall-id
           (cons (match-lambda
                   [(struct-id args ...)
                    #t]
                   [_ #f])
                 (match-lambda
                   [(struct-id args ...)
                    (record* (quote struct-id) args ...)])))
         (define struct-unmarshall-id
           (cons (lambda (label)
                   (eq? label (quote struct-id)))
                 struct-id))))


(define-recordable-struct op:bootstrap
  (question-id)
  marshall::op:bootstrap unmarshall::op:bootstrap)

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-recordable-struct op:deliver-only
  (;; Position in the table for the target
   ;; (sender's imports, reciever's exports)
   target-pos
   ;; Either the method name, or #f if this is a procedure call
   method
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
   args
   kw-args)
  marshall::op:deliver-only unmarshall::op:deliver-only)

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-recordable-struct op:deliver
  (answer-pos
   redirector  ; a resolver...?
   target-pos
   method
   args)
  marshall::op:deliver unmarshall::op:deliver)

(define-recordable-struct op:abort
  (reason)
  marshall::op:abort unmarshall::op:abort)

;; Effectively splitting "return" into two distinct operations
(define-recordable-struct op:return-fulfill
  (answer-id val)
  marshall::op:return-fulfill unmarshall::op:return-fulfill)
(define-recordable-struct op:return-break
  (answer-id problem)
  marshall::op:return-break unmarshall::op:return-break)

(define-recordable-struct desc:new-far-desc
  (import-pos)
  marshall::desc:new-far-desc unmarshall::desc:new-far-desc)

(define-recordable-struct desc:new-remote-promise
  (import-pos
   redirector-pos) ;; ?
  marshall::desc:new-remote-promise unmarshall::desc:new-remote-promise)


;; (define op:bootstrap->record
;;   (match-lambda
;;     [(op:bootstrap question-id)
;;      (record* 'op:bootstrap question-id)]))
;; (define op:deliver-only->record
;;   (match-lambda
;;     [(op:deliver-only target-pos method args kw-args)
;;      (record* 'op:deliver-only target-pos method args kw-args)]))


(define (make-marshalling-pair convert-val-to-slot convert-slot-to-val)
  'TODO
  )


(define (^craptp bcom in-port out-port)
  #;(syscaller-free-thread)
  'TODO
  )


(module+ test
  (require rackunit)
  (define vat-a
    (make-vat))

  (define-values (ctp-ip ctp-op)
    (make-pipe))

  )
