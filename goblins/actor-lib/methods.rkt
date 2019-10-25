#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/match)
         racket/contract
         syntax/parse
         "../core.rkt"
         "select-swear.rkt")

(provide methods)

(define raise-method-not-found
  (make-keyword-procedure
   (lambda (kws kw-args method . args)
     (error 'method-not-found "~a" method))))

(define/contract (make-extends-handler extend-refr)
  (-> refr? any/c)
  (define $/<-p
    (select-$/<-p extend-refr))
  (define extends-handler
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (keyword-apply $/<-p kws kw-args extend-refr args))))
  extends-handler)

(define-syntax (methods stx)
  (define-values (case-clauses method-not-found-handler)
    (let lp ([stx-to-process (cdr (syntax-e stx))]
             [clauses '()]
             [not-found-handler #f])
      (match stx-to-process
        ['()
         (values (reverse clauses)
                 (or not-found-handler
                     #'raise-method-not-found))]
        [(list clause rest-clauses ...)
         (define clause-e
           (syntax-e clause))
         (cond
           ;; Okay, we're setting up an fallback definition
           [(eq? clause-e '#:fallback)
            (match rest-clauses
              [(list not-found-handler rest-clauses ...)
               (lp rest-clauses
                   clauses
                   not-found-handler)]
              ['()
               (raise-syntax-error
                'methods-fallback-definition
                "#:fallback must be followed by a procedure")])]
           [(eq? clause-e '#:extend)
            (match rest-clauses
              [(list extends-refr rest-clauses ...)
               (lp rest-clauses
                   clauses
                   #`(make-extends-handler #,extends-refr))]
              ['()
               (raise-syntax-error
                'methods-invalid-extends-refr
                "#:extend must be followed by a refr")])]
           [else
            (define new-clause
              (syntax-parse clause
                [((method-name:id method-args ...) body ...)
                 #'('method-name
                    (procedure-rename
                     (lambda (method-args ...)
                       body ...)
                     'method-name))]
                [((method-name:id method-args ... . rest) body ...)
                 #'('method-name
                    (procedure-rename
                     (lambda (method-args ... . rest)
                       body ...)
                     'method-name))]
                [(method-name:id proc)
                 #'('method-name proc)]))
            (lp rest-clauses
                (cons new-clause clauses)
                not-found-handler)])])))
  
  #`(procedure-rename
     (make-keyword-procedure
      (lambda (kws kw-args method . args)
        (define method-proc
          (case method
            #,@case-clauses
            [else #f]))
        (if method-proc
            (keyword-apply method-proc kws kw-args args)
            (#,method-not-found-handler method))))
     'methods))

(module+ test
  (require rackunit)
  (define some-methods
    (methods
     [(foo x)
      (list 'foo x)]
     [bar
      (lambda (x)
        (list 'bar x))]
     [(baz . args)
      (list 'baz args)]))
  (check-equal?
   (some-methods 'foo 'beep)
   '(foo beep))
  (check-equal?
   (some-methods 'bar 'beep)
   '(bar beep))
  (check-equal?
   (some-methods 'baz 'beep 'boop 'bop)
   '(baz (beep boop bop)))

  (define some-methods-with-custom-fallback
    (methods
     #:fallback
     (make-keyword-procedure
      (lambda _ 'haha-fallback))
     [(foo x)
      (list 'foo x)]
     [bar
      (lambda (x)
        (list 'bar x))]
     [(baz . args)
      (list 'baz args)]))

  (check-equal?
   (some-methods-with-custom-fallback 'blorp)
   'haha-fallback)

  (define am
    (make-actormap))

  (define fallback-to-me
    (actormap-spawn! am (lambda (bcom)
                          (methods
                           [(foo)
                            'i-am-foo]))))
  (define extends-fallback-to-me
    (actormap-spawn! am (lambda (bcom)
                          (methods
                           #:extend fallback-to-me
                           [(bar)
                            'i-am-bar]))))
  (check-eq?
   (actormap-peek am extends-fallback-to-me 'bar)
   'i-am-bar)
  (check-eq?
   (actormap-peek am extends-fallback-to-me 'foo)
   'i-am-foo))
