#lang racket

(require goblins
         racket/match)

(define (do-actors-gc [actormap (make-whactormap)])
  (define ((simple-actor bcom))
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
  (define (^simple-actor bcom)
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
          (spawn ^simple-actor))
        (call friend 'oop))))))

;;; 2019-10-29
;; perf-tests.rkt> (call-a-lot)
;; cpu time: 991 real time: 990 gc time: 5

(define (call-a-lot [actormap (make-whactormap)])
  (define ((^simple-actor bcom))
    'hello)
  (time
   (actormap-run!
    actormap
    (lambda ()
      (define friend
        (spawn ^simple-actor))
      (for ([i 1000000])
        (call friend))))))

;;; 2019-11-03
;; perf-tests.rkt> (bcom-a-lot)
;; cpu time: 1473 real time: 1472 gc time: 49
;; perf-tests.rkt> (bcom-a-lot #:reckless? #t)
;; cpu time: 1262 real time: 1262 gc time: 24

;; A bunch of actors updating themselves
(define (bcom-a-lot [actormap (make-whactormap)]
                    #:num-actors [num-actors 1000]
                    #:iterations [iterations 1000]
                    #:reckless? [reckless? #f])
  (define ((^incrementing-actor bcom [i 0]))
    (values (bcom (^incrementing-actor bcom (add1 i)))
            i))
  (define i-as
    (for/list ([i num-actors])
      (actormap-spawn! actormap ^incrementing-actor)))
  (time
   (for ([_ iterations])
     (actormap-run!
      actormap
      (lambda ()
        (for ([i-a i-as])
          ($ i-a)))
      #:reckless? reckless?))))

;;; 2019-10-29
;; perf-tests.rkt> (set!-a-lot)
;; cpu time: 1106 real time: 1105 gc time: 13

(define (set!-a-lot [actormap (make-whactormap)]
                    #:num-actors [num-actors 1000]
                    #:iterations [iterations 1000])
  (define (^incrementing-actor bcom)
    (define i 0)
    (lambda ()
      (define old-i i)
      (set! i (add1 i))
      old-i))
  (define i-as
    (for/list ([i num-actors])
      (actormap-spawn! actormap ^incrementing-actor)))
  (time
   (for ([_ iterations])
     (actormap-run!
      actormap
      (lambda ()
        (for ([i-a i-as])
          ($ i-a)))))))

