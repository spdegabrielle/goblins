#lang racket/base

(require "../core.rkt")

(provide ^imply-method)

(define (^imply-method bcom wrap-me method)
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (keyword-apply call kws kw-args wrap-me method args))))

(module+ test
  (require rackunit
           racket/match)
  (define am (make-actormap))
  (define (^foo-or-bar bcom)
    (match-lambda*
      [(list 'foo arg1 arg2)
       (list 'got-foo arg1 arg2)]
      [(list 'bar arg1 arg2)
       (list 'got-bar arg1 arg2)]))
  (define foo-or-bar (actormap-spawn! am ^foo-or-bar))
  (define fob-imp (actormap-spawn! am ^imply-method foo-or-bar 'foo))
  (check-equal?
   (actormap-peek am fob-imp 'beep 'boop)
   '(got-foo beep boop)))
