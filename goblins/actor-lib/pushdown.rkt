#lang racket/base

(provide spawn-pushdown-pair)

(require "../core.rkt"
         "select-swear.rkt"
         racket/match)

(define (spawn-pushdown-pair [initial-refr #f])
  (define-cell stack
    (if initial-refr
        (list initial-refr)
        '()))
  (define (^pd-stack bcom)
    (make-keyword-procedure
     (lambda (kws kw-args method . args)
       (define method-proc
         (case method
           ['push
            (lambda (refr)
              ;; Add to the stack
              ($ stack (cons refr ($ stack)))
              (void))]
           ['spawn-push
            (make-keyword-procedure
             (lambda (kws kw-args constructor . args)
               (define cur-stack
                 ($ stack))
               (define stack-top
                 (match cur-stack
                   [(cons stack-top rest-stack)
                    stack-top]
                   ['() #f]))
               (define new-refr
                 (keyword-apply spawn kws kw-args constructor
                                stack-top args))
               ($ stack (cons new-refr cur-stack))
               new-refr))]
           ['pop
            (lambda ()
              (match ($ stack)
                [(cons stack-top rest-stack)
                 ;; set stack to the remaining value
                 ($ stack rest-stack)
                 ;; return the top value
                 ;; TODO: Is this actually useful?
                 stack-top]
                ['() (error "Empty stack")]))]
           ;; For debugging should we add a method that allows us to
           ;; see the stack contents?  Is that ever a problem?
           ['empty?
            (lambda ()
              (null? ($ stack)))]))
       (keyword-apply method-proc kws kw-args args))))
  (define (^pd-forwarder bcom)
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (match ($ stack)
         [(cons stack-top rest-stack)
          (keyword-apply run-$/<-p kws kw-args stack-top args)]))))
  (list (spawn ^pd-stack) (spawn ^pd-forwarder)))

(module+ test
  (require rackunit)
  (define am (make-actormap))

  (match-define (list pd-stack pd-forwarder)
    (actormap-run! am spawn-pushdown-pair))

  (define first-actor
    (actormap-poke! am pd-stack 'spawn-push
                    (lambda (bcom prev foo)
                      (lambda (bar)
                        (list 'first prev foo bar)))
                    'foo))
  (check-equal?
   (actormap-poke! am pd-forwarder 'bar)
   `(first #f foo bar))
  (define second-actor
    (actormap-poke! am pd-stack 'spawn-push
                    (lambda (bcom prev foo)
                      (lambda (bar)
                        (list 'second prev foo bar)))
                    'foo2))
  (check-equal?
   (actormap-poke! am pd-forwarder 'bar2)
   `(second ,first-actor foo2 bar2))
  
  (check-not-exn
   (lambda ()
     (actormap-poke! am pd-stack 'pop)))

  (check-equal?
   (actormap-poke! am pd-forwarder 'bar3)
   `(first #f foo bar3)))

