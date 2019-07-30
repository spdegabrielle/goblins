#lang racket

;; Add a "ticking" object, where objects can register to be ticked
;; eg on a game loop.  If the called 'tick method responds with
;; 'die the actor will not be queued for ticking again.

(require "../core.rkt"
         racket/match)

(provide spawn-ticker-pair)

(define (spawn-ticker-pair)
  (define new-ticked
    (spawn (make-cell '())))
  ;; This registers new ticked objects
  (define (tick-register bcom . entries)
    (new-ticked (append entries (new-ticked))))
  ;; This runs all ticked objects
  (define ((make-ticker current-ticked) bcom)
    ;; Update set of tickers with any that have been
    ;; added since when we last ran
    (define updated-ticked
      (append (new-ticked) current-ticked))
    ;; reset new-ticked
    (new-ticked '())
    ;; Now run all ticked objects
    (define next-tickers
      (foldr (lambda (tick-me next-queue)
               (match (tick-me)
                 ['die next-queue]
                 [_ (cons tick-me next-queue)]))
             '()
             updated-ticked))
    ;; update ourself
    (bcom (make-ticker next-tickers)))
  (list (spawn tick-register)
        (spawn (make-ticker '()))))

(module+ test
  (require rackunit)

  (define am (make-actormap))
  (match-define (list register-ticker ticker-tick)
    (actormap-run! am spawn-ticker-pair))
  (define joe-speaks-here
    (actormap-spawn! am (make-cell)))
  (define jane-speaks-here
    (actormap-spawn! am (make-cell)))
  (define (malaise-sufferer name speaking-cell
                            [maximum-suffering 3])
    (define ((loop n) bcom)
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
            (bcom (loop (add1 n))))))
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
