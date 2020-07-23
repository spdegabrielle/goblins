#lang racket/base

(provide make-sealer-triplet)

(require racket/match)

;; This approach inspired by W7's use of "records as sealers"
;; see w7-fun.scm in the W7 (by Jonathan Rees) repo

; Create a new sealer / unsealer pair and sealed? predicate
; sealer-name is optional and just for debugging help
(define (make-sealer-triplet [sealer-name #f])
  (define struct-name
    (match sealer-name
      [#f 'sealed]
      [(or (? string?) (? symbol?))
       (string->symbol (format "sealed-by-~a" sealer-name))]))
  (define-values (struct:seal seal sealed? seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))
  (values seal unseal sealed?))
