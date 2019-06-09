#lang racket/base

(provide make-cell
         spawn-cell)

(require "../core.rkt")

;; A simple turn-mutable cell

(define (make-cell [val #f])
  (case-lambda
    [() val]
    [(new-val)
     (next (make-cell new-val))]))

(define (spawn-cell [val #f])
  (spawn (make-cell val)))

(module+ test
  (require rackunit)
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
