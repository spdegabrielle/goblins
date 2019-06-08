#lang racket

;; Add a "ticking" object, where objects can register to be ticked
;; eg on a game loop.  If the called 'tick method responds with
;; 'die the actor will not be queued for ticking again.

(require "../core.rkt"
         "cell.rkt"
         racket/match)

(define (spawn-ticker-pair)
  (define tick-queue
    (spawn (make-cell '())))
  (define (register-ticker . entries)
    (tick-queue
     (for/fold ([tickers (tick-queue)])
               ([entry entries])
       (cons entry tickers))))
  (define ticker-tick
    (lambda ()
      (define next-queue
        (foldr (lambda (tick-me next-queue)
                 (match (tick-me 'tick)
                   ['die next-queue]
                   [_ (cons tick-me next-queue)]))
               '()
               (tick-queue)))
      (tick-queue next-queue)))
  (list (spawn register-ticker) (spawn ticker-tick)))

(module+ test
  (require rackunit
           "masyme.rkt")

  (define am (make-actormap))
  (match-define (list register-ticker ticker-tick)
    (actormap-run! am spawn-ticker-pair))
  (define joe-speaks-here
    (actormap-spawn! am (make-cell)))
  (define jane-speaks-here
    (actormap-spawn! am (make-cell)))
  (define (malaise-sufferer name speaking-cell
                            [maximum-suffering 3])
    (define (loop n)
      (masyme
       [(tick)
        (if (> n maximum-suffering)
            (begin
              (speaking-cell
               (format "<~a> you know what? I'm done."
                       name))
              'die)
            (begin
              (speaking-cell
               (format "<~a> sigh number ~a"
                       name n))
              (next (loop (add1 n)))))]))
    (loop 1))
  (define joe
    (actormap-spawn! am (malaise-sufferer "joe"
                                          joe-speaks-here)))
  (define jane
    (actormap-spawn! am (malaise-sufferer "jane"
                                          jane-speaks-here
                                          2)))
  (actormap-poke! am register-ticker joe jane)
  (actormap-poke! am ticker-tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 1")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> sigh number 1")

  (actormap-poke! am ticker-tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 2")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> sigh number 2")

  (actormap-poke! am ticker-tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 3")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> you know what? I'm done.")

  (actormap-poke! am ticker-tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> you know what? I'm done.")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> you know what? I'm done."))
