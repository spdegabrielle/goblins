#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/match)
         syntax/parse)

(define-syntax (methods stx)
  (define case-clauses
    (match (syntax-e stx)
      [(list _ method-defns ...)
       (map (lambda (stx)
              (syntax-parse stx
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
            method-defns)]))
  
  #`(procedure-rename
     (make-keyword-procedure
      (lambda (kws kw-args method . args)
        (define method-proc
          (case method
            #,@case-clauses
            [else
             (error 'method-not-found "~a" method)]))
        (keyword-apply method-proc kws kw-args args)))
     'methods))

(module+ test
  (require rackunit
           "../core.rkt"))
