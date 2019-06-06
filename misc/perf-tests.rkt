#lang racket

(require goblinoid
         racket/match)

(define (do-actors-gc actormap)
  (define (simple-actor)
    'hello)
  (time
   (actormap-run!
    actormap
    (lambda ()
      (for ([i 1000000])
        (define friend
          (spawn simple-actor))
        (call friend))))))

;; (collect-garbage 'major)

(define (do-self-referential-actors-gc actormap)
  (define (make-simple-actor)
    (define self
      (match-lambda*
        [(list 'self) self]
        [(list 'oop) 'boop]))
    self)
  (time
   (actormap-run!
    actormap
    (lambda ()
      (for ([i 1000000])
        (define friend
          (spawn (make-simple-actor)))
        (call friend 'oop))))))

(define (call-a-lot actormap)
  (define (simple-actor)
    'hello)
  (time
   (actormap-run!
    actormap
    (lambda ()
      (define friend
        (spawn simple-actor))
      (for ([i 1000000])
        (call friend))))))
