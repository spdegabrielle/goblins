#lang racket/base

(require racket/contract)

(provide bookkeeper)

;; TODO: go in its own module?
(struct bookkeeper
  #:mutable
  ;; My reading on this is a bit different than captp.js but I'm
  ;; not sure why
  (;; References we have to remote objects
   ;;   {remote-refr?: integer?}
   imports            ; (and/c hash-eq? hash-weak?)
   ;; Stuff we're exporting
   ;;   {any/c: integer?}
   exports:val->slot  ; hash-eqv?
   ;;   {integer?: any/c}
   exports:slot->val  ; hash-eq?

   ;; Local promises
   ;;   {(and/c local-refr? local-promise?): integer?}
   questions
   ;;   {integer?: (and/c local-refr? promise-refr?)}
   answers

   ;; TODO: Not sure if we need this one
   #;last-promise-id
   last-export-id

   ;; integer?
   last-question-id))

(define (new-bookkeeper))

(define/contract (bookkeeper-add/get-import! bkeep slot-id debug-name)
  (-> bookkeeper? integer? any/c)
  'TODO)

(define/contract (bookkeeper-add/get-export! bkeep obj)
  (-> bookkeeper? any/c integer?)
  'TODO)

;;; Question making and resolution
;;; ==============================

;; This one needs to coordinate with the vat where the promise will be
;; kept.  We get the resolver from a promise pair so we can resolve that
;; when it comes in (that's all we actually care about, since those who
;; want the promise refr should already be able to get it).
;; Returns the slot id.
(define/contract (bookkeeper-add/get-question! bkeep resolver-ref)
  (-> bookkeeper? live-refr? live-refr?
      integer?)
  'TODO)

;; Incoming resolution to this question
(define/contract (bookkeeper-resolve-question! bkeep slot-id resolution)
  (-> bookkeeper? integer? any/c
      any/c)
  'TODO)


;;; Now we've got our answers.


;;; New incoming thing for us to answer.  We presumbably don't have
;;; the answer yet by this point, but we need to spawn a resolver-like
;;; object that can give us the answer we need.
;;; What we return is a sealer that's allowed to provide the answer.
(define/contract (bookkeeper-answer-add/get-answer-id! bkeep slot-id)
  (-> bookkeeper? integer?
      any/c) ; the sealer, but we don't have a good "type" for this
  'TODO)

(define/contract (bookkeeper-resolve-answer! bkeep slot-id sealed-resolution
                                              send-resolution)
  (-> bookkeeper? integer? any/c procedure?
      any/c)
  'TODO)
