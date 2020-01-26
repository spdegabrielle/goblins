#lang racket/base

(require (for-syntax syntax/parse racket/base)
         "../core.rkt")

(provide ^cell
         spawn-cell
         cell->read-only
         cell->write-only
         define-cell)

;;; Cells
;;; =====

;; A simple turn-mutable cell

;; Constructor for a cell.  Takes an optional initial value, defaults
;; to false.
(define (^cell bcom [val #f])
  (case-lambda
    ;; Called with no arguments; return the current value
    [() val]
    ;; Called with one argument, we become a version of ourselves
    ;; with this new value
    [(new-val)
     (bcom (^cell bcom new-val))]))

(define (spawn-cell [val #f])
  (spawn ^cell val))

(define (cell->read-only cell)
  (spawn-proc (lambda () (cell))))

(define (cell->write-only cell)
  (spawn-proc (lambda (new-val) (cell new-val))))

(define-syntax (define-cell stx)
  (syntax-parse stx
    [(_ id:id)
     #'(define id
         (spawn (procedure-rename ^cell 'id)))]
    [(_ id:id val)
     #'(define id
         (spawn (procedure-rename ^cell 'id) val))]))

(module+ test
  (require rackunit)
  (define am (make-whactormap))

  (define a-cell
    (actormap-spawn! am ^cell))
  (test-eq?
   "cell without default value and unset is #f"
   (actormap-peek am a-cell)
   #f)
  (actormap-poke! am a-cell 'foo)
  (test-eq?
   "cell after being set retains value"
   (actormap-peek am a-cell)
   'foo)
  
  (test-eq?
   "cell default values"
   (actormap-peek am (actormap-spawn! am ^cell 'hello))
   'hello))

