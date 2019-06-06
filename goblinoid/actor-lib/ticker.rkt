#lang racket

(require "../main.rkt"
         "symethods.rkt"
         "cell.rkt"
         racket/match)

(define (spawn-ticker-pair)
  (define tick-set
    (spawn (make-cell (seteq))))
  (define ticker-registry
    (spawn-symethods
     [(register . entries)
      (for ([entry entries])
        (define current-tickers
          (call tick-set))
        (call tick-set (set-add current-tickers
                                entry)))]
     [(remove entry)
      (call tick-set
            (set-remove (call tick-set) entry))]))
  (define ticker-tick
    (spawn (lambda ()
             (for ([tick-me (call tick-set)])
               (call tick-me 'tick)))))
  (list ticker-registry ticker-tick))

(module+ test
  (require rackunit)

  (define am (make-actormap))
  (match-define (list ticker-registry ticker-tick)
    (actormap-run! am spawn-ticker-pair))
  (define joe-speaks-here
    (actormap-spawn! am (make-cell)))
  (define jane-speaks-here
    (actormap-spawn! am (make-cell)))
  (define (malaise-sufferer name speaking-cell)
    (define (loop n)
      (symethods
       [(tick)
        (call speaking-cell
              (format "<~a> sigh number ~a"
                      name n))
        (values (void) (loop (add1 n)))]))
    (loop 1))
  (define joe
    (actormap-spawn! am (malaise-sufferer "joe"
                                          joe-speaks-here)))
  (define jane
    (actormap-spawn! am (malaise-sufferer "jane"
                                          jane-speaks-here)))
  (actormap-poke! am ticker-registry 'register joe jane)
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
   "<jane> sigh number 2"))
