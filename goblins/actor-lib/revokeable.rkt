#lang racket/base

(provide spawn-revokeable)

(require "../core.rkt"
         "cell.rkt"
         "select-swear.rkt")

(define (spawn-revokeable target)
  (define $/<- (select-$/<- target))
  (define revoked?
    (spawn-cell #f))
  (define (^forwarder bcom)
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (when ($ revoked?)
         (error "Access revoked!"))
       (keyword-apply $/<- kws kw-args target args))))
  (define ((^revoker bcom))
    ($ revoked? #t))
  (list (spawn ^forwarder) (spawn ^revoker)))

(module+ test
  (require rackunit
           racket/match
           racket/contract)
  (define am (make-actormap))
  (define royal-admission
    (actormap-spawn!
     am (lambda (bcom)
          (lambda _
            "The Queen will see you now."))))
  (match-define (list royal-forwarder royal-revoker)
    (actormap-run! am
                   (lambda ()
                     (spawn-revokeable royal-admission))))

  (check-equal?
   (actormap-peek am royal-forwarder)
   "The Queen will see you now.")

  (actormap-poke! am royal-revoker)

  (check-exn
   any/c
   (lambda () (royal-forwarder))))
