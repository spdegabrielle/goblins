#lang racket/base

(require racket/match)

(provide match-methods match-methods*)

(define-syntax-rule (match-methods become [(method rest ...) body ...] ...)
  (match-lambda*
    [(list become 'method rest ...)
     body ...] ...))

(define-syntax-rule (match-methods* become [(rest ...) body ...] ...)
  (match-lambda*
    [(list become rest ...)
     body ...] ...))

(module+ test
  (require rackunit)
  (define (cell [val #f])
    (match-methods
     become
     [(get) val]
     [(set new-val)
      (become new-val)]))

  (define a-cell (cell 1))
  (check-equal? (a-cell list 'get) 1)
  (check-equal? (a-cell list 'set 2) '(2)))
