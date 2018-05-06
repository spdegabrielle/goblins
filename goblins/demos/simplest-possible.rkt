#lang racket/load

(require "../actors.rkt")

(define (main . args)
  (define done? (make-semaphore))
  (define proog
    (spawn
     (lambda ()
       (display "proog> Listen, Emo!  Listen to the sounds of the machine!\n")
       (semaphore-post done?))))
  (define emo
    (spawn
     (lambda (target)
       (display "emo> What's next, Proog?\n")
       (<- target))))
  (<- emo proog)
  (semaphore-wait done?))
