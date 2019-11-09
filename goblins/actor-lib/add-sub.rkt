#lang racket/base

;; Or $cell-add1 / $cell-sub1 ...?
(provide cell-add1 cell-sub1)

(require goblins)

(define (cell-add1 cell)
  ($ cell (add1 ($ cell)))
  (void))

(define (cell-sub1 cell)
  ($ cell (sub1 ($ cell)))
  (void))
