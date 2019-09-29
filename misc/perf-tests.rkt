#lang racket

(require goblins
         racket/match)

(define (do-actors-gc [actormap (make-whactormap)])
  (define (simple-actor bcom)
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

(define (do-self-referential-actors-gc [actormap (make-whactormap)])
  (define (make-simple-actor bcom)
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

(define (call-a-lot [actormap (make-whactormap)])
  (define (simple-actor bcom)
    'hello)
  (time
   (actormap-run!
    actormap
    (lambda ()
      (define friend
        (spawn simple-actor))
      (for ([i 1000000])
        (call friend))))))

;; A bunch of actors updating themselves
(define (bcom-a-lot [actormap (make-whactormap)]
                    #:num-actors [num-actors 1000]
                    #:iterations [iterations 1000])
  (define ((incrementing-actor [i 0]) bcom)
    (bcom (incrementing-actor (add1 i)) i))
  (define i-as
    (for/list ([i num-actors])
      (actormap-spawn! actormap (incrementing-actor))))
  (time
   (for ([_ iterations])
     (actormap-run!
      actormap
      (lambda ()
        (for ([i-a i-as])
          (i-a)))))))
