#lang racket/base

(provide spawn-pushdown-pair)

(require "../core.rkt"
         "select-swear.rkt"
         racket/match)

(define (spawn-pushdown-pair [initial-refr #f])
  (define stack
    (spawn (procedure-rename ^cell '^pushdown-stack)
           (if initial-refr
               (list initial-refr)
               '())))
  (define (^pushdown bcom)
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
                   [(list stack-top rest-stack ...)
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
                [(list stack-top rest-stack ...)
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
  (define (^automata bcom)
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (match ($ stack)
         [(list stack-top rest-stack ...)
          (keyword-apply run-$/<-p kws kw-args stack-top args)]))))
  (list (spawn ^pushdown) (spawn ^automata)))

(module+ test
  (require rackunit)
  (define am (make-actormap))

  (match-define (list pushdown automata)
    (actormap-run! am spawn-pushdown-pair))

  (define first-actor
    (actormap-poke! am pushdown 'spawn-push
                    (lambda (bcom prev foo)
                      (lambda (bar)
                        (list 'first prev foo bar)))
                    'foo))
  (check-equal?
   (actormap-poke! am automata 'bar)
   `(first #f foo bar))
  (define second-actor
    (actormap-poke! am pushdown 'spawn-push
                    (lambda (bcom prev foo)
                      (lambda (bar)
                        (list 'second prev foo bar)))
                    'foo2))
  (check-equal?
   (actormap-poke! am automata 'bar2)
   `(second ,first-actor foo2 bar2))
  
  (check-not-exn
   (lambda ()
     (actormap-poke! am pushdown 'pop)))

  (check-equal?
   (actormap-poke! am automata 'bar3)
   `(first #f foo bar3)))

