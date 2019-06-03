#lang racket

(provide new-seal)

;; From http://erights.org/elib/capability/ode/ode-capabilities.html

; Create a new sealer / unsealer pair and sealed? predicate
; sealer-name is optional and just for debugging help
(define (new-seal [sealer-name #f])
  (define struct-name
    (if sealer-name
        (string->symbol (string-append "sealed-by-" (symbol->string sealer-name)))
        'sealed))
  (define-values (struct:seal seal sealed? seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))
  (values seal unseal sealed?))
