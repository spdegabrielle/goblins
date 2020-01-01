#lang racket

;; Add a "ticking" object, where objects can be spawne so that they
;; can be ticked on eg a game loop.

(require "../core.rkt"
         "methods.rkt"
         racket/match)

(provide spawn-ticker)

(define (spawn-ticker)
  (define-cell new-ticked
    '())

  (define (to-tick give-ticky)
    (define ticky
      (spawn ^ticky #f))
    (define new-refr
      (give-ticky ticky))
    ($ new-ticked
       (cons (vector new-refr ticky) ($ new-ticked)))
    new-refr)

  (define (^ticky bcom dead?)
    (methods
     [(die)
      (bcom (^ticky bcom #t))]
     [(dead?)
      dead?]
     [to-tick to-tick]))

  (define (^ticker bcom current-ticked)
    (methods
     [to-tick to-tick]

     ;; This wonky looking procedure actually does the ticking.
     ;; We apply any arguments given to the tick method to all
     ;; refrs that aren't dead according to their ticky.  And if
     ;; they're still not dead, then we queue them up for next
     ;; time.
     [tick
      (make-keyword-procedure
       (lambda (kws kw-args . args)
         ;; Update set of tickers with any that have been
         ;; added since when we last ran
         (define updated-ticked
           (append ($ new-ticked) current-ticked))
         ;; reset new-ticked
         ($ new-ticked '())

         ;; Now run all ticked objects
         ;; (@@: The natural iteration makes this not so easy to read.
         ;;   Maybe it's worth a rewrite for cleanliness?  Dunno.)
         (define next-tickers
           (let lp ([to-tick updated-ticked])
             (match to-tick
               ['() '()]
               [(cons this-ticked tick-rest)
                (match this-ticked
                  [(vector ticked-refr ticked-ticky)
                   (if ($ ticked-ticky 'dead?)
                       ;; continue, it's dead
                       (lp tick-rest)
                       ;; otherwise, let's tick it
                       (begin
                         (keyword-apply $ kws kw-args ticked-refr args)
                         (if ($ ticked-ticky 'dead?)
                             ;; well it wasn't dead before, but it is now
                             (lp tick-rest)
                             ;; ok it's dead now too
                             (cons this-ticked
                                   (lp tick-rest)))))])])))
         (bcom (^ticker bcom next-tickers))))]
     ;; Used for collision detection, etc.
     [(foldr proc init)
      (foldr (match-lambda*
               [(list (vector refr ticky) prev)
                ;; skip if dead (probably from a previous foldr)
                (if ($ ticky 'dead?)
                    prev
                    (proc refr prev))])
             init current-ticked)]))

  (spawn ^ticker '()))

(module+ test
  (require rackunit)

  (define am (make-actormap))

  (define ticker (actormap-run! am spawn-ticker))

  (define joe-speaks-here
    (actormap-spawn! am ^cell))
  (define jane-speaks-here
    (actormap-spawn! am ^cell))
  (define (^malaise-sufferer bcom ticky name speaking-cell
                             [maximum-suffering 3])
    (define ((loop n))
      (if (> n maximum-suffering)
          (begin
            ($ speaking-cell
               (format "<~a> you know what? I'm done."
                       name))
            ($ ticky 'die))
          (begin
            ($ speaking-cell
               (format "<~a> sigh number ~a"
                       name n))
            (bcom (loop (add1 n))))))
    (loop 1))
  (define joe
    (actormap-poke! am ticker 'to-tick
                    (lambda (ticky)
                      (spawn ^malaise-sufferer ticky "joe"
                             joe-speaks-here))))
  (define jane
    (actormap-poke! am ticker 'to-tick
                    (lambda (ticky)
                      (spawn ^malaise-sufferer ticky "jane"
                             jane-speaks-here
                             2))))
  (actormap-poke! am ticker 'tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 1")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> sigh number 1")

  (actormap-poke! am ticker 'tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 2")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> sigh number 2")

  (actormap-poke! am ticker 'tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> sigh number 3")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> you know what? I'm done.")

  (actormap-poke! am ticker 'tick)
  (check-equal?
   (actormap-peek am joe-speaks-here)
   "<joe> you know what? I'm done.")
  (check-equal?
   (actormap-peek am jane-speaks-here)
   "<jane> you know what? I'm done.")

  )
