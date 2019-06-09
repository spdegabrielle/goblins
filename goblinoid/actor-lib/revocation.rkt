#lang racket/base

(require "../core.rkt"
         "cell.rkt")

(define (spawn-revokeable target)
  (define revoked?
    (make-cell #f))
  (define forwarder
    (spawn
     (make-keyword-procedure
      (lambda (kws kw-args . args)
        (when (revoked?)
          (error "Access revoked!"))
        (keyword-apply target kws kw-args args)))))
  (define revoker
    (spawn
     (lambda ()
       (revoked? #t))))
  (list forwarder revoker))

(module+ test
  (require rackunit
           racket/match
           racket/contract)
  (define am (make-actormap))
  (define royal-admission
    (actormap-spawn!
     am (lambda ()
          "The Queen will see you now.")))
  (match-define (list royal-forwarder royal-revoker)
    (actormap-run! am (lambda ()
                        (spawn-revokeable royal-admission))))

  (check-equal?
   (actormap-peek am royal-forwarder)
   "The Queen will see you now.")

  (actormap-poke! am royal-revoker)

  (check-exn
   any/c
   (lambda () (royal-forwarder))))
