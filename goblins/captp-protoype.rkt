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
   args
   kw-args)
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

(define marshallers
  (list marshall::op:bootstrap
        marshall::op:deliver-only
        marshall::op:deliver
        marshall::op:abort
        marshall::op:return-fulfill
        marshall::op:return-break
        marshall::desc:new-far-desc
        marshall::desc:new-remote-promise))

(define unmarshallers
  (list unmarshall::op:bootstrap
        unmarshall::op:deliver-only
        unmarshall::op:deliver
        unmarshall::op:abort
        unmarshall::op:return-fulfill
        unmarshall::op:return-break
        unmarshall::desc:new-far-desc
        unmarshall::desc:new-remote-promise))


(define (^craptp-conn bcom in-port out-port
                      ;; We need a way to send messages to ourselves from the
                      ;; incoming message listener thread (promises not used, so
                      ;; a <-np equivalent is good).
                      ;; The easiest way to do this is to reuse an existing
                      ;; vat dispatcher reference.
                      self-send-np)
  (syscaller-free-thread
   (lambda ()
     (define last-export-id 0)
     ;; (define last-question-id 0)
     ;; (define last-promise-id 0)

     (define exports-val2slot (make-weak-hasheq))
     (define exports-slot2val (make-hasheqv))
     (define imports (make-hasheqv))
     ;; (define questions (make-hasheqv))
     ;; (define answers (make-hasheqv))

     (define (next-message)
       (syrup-read in-port #:unmarshallers unmarshallers))

     (call/ec
      (lambda (escape-lp)
        (let lp ()
          (match (pk 'next-message (next-message))
            [(op:bootstrap question-id)
             (pk 'bootstrapped)
             'TODO]
            [(op:deliver-only target-pos method args kw-args)
             (pk 'deliver-onlyed)
             'TODO]
            [(op:deliver answer-pos redirector target-pos method args kw-args)
             (pk 'delivered)
             'TODO]
            [(op:abort reason)
             (pk 'aborted)
             'TODO])
          (lp))))))
  'TODO
  )


(module+ test
  (require rackunit)
  (define vat-a
    (make-vat))
  (define vat-b
    (make-vat))

  (define-values (a->b-ip a->b-op)
    (make-pipe))
  (define-values (b->a-ip b->a-op)
    (make-pipe))

  (define ((^greeter bcom my-name) your-name)
    (format "<~a> Hello ~a!" my-name your-name))

  (define alice
    (vat-a 'spawn ^greeter "Alice"))



  )
