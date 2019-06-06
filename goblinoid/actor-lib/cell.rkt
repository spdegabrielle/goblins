#lang racket/base

(provide make-cell)

;; A simple turn-mutable cell

(define (make-cell [val #f])
  (case-lambda
    [() val]
    [(new-val)
     (values (void) (make-cell new-val))]))

(module+ test
  (require rackunit
           "../main.rkt")
  (define am (make-actormap))
  (define a-cell
    (actormap-spawn! am (make-cell)))
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
   (actormap-peek am (actormap-spawn! am (make-cell 'hello)))
   'hello))
