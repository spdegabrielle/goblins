#lang racket

;; Add a "ticking" object, where objects can be spawne so that they
;; can be ticked on eg a game loop.

(require "../core.rkt"
         "methods.rkt"
         racket/match)

(provide spawn-ticker)

(define (spawn-ticker)
  (define new-ticked
    (spawn ^cell '()))

  (define spawn-ticked
    (make-keyword-procedure
     (lambda (kws kw-args constructor . args)
       (define ticky
         (spawn ^ticky #f))
       (define new-refr
         (keyword-apply spawn kws kw-args constructor ticky args))
       ($ new-ticked
          (cons (vector new-refr ticky) ($ new-ticked)))
       new-refr)))

  (define (^ticky bcom dead?)
    (methods
     [(die)
      (bcom ^ticky #t)
      (void)]
     [(dead?)
      dead?]
     [spawn spawn-ticked]))

  (define (^ticker bcom current-ticked)
    (methods
     [spawn spawn-ticked]

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
               [(list this-ticked tick-rest ...)
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
         (bcom ^ticker next-tickers)))]))

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
    (define ((loop bcom n))
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
            (bcom loop (add1 n)))))
    (loop bcom 1))
  (define joe
    (actormap-poke! am ticker 'spawn
                    ^malaise-sufferer "joe"
                    joe-speaks-here))
  (define jane
    (actormap-poke! am ticker 'spawn
                    ^malaise-sufferer "jane"
                    jane-speaks-here
                    2))
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
