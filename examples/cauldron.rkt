#lang racket

(require lux
         (prefix-in raart: raart)
         racket/match)

(require goblinoid
         goblinoid/actor-lib/env
         goblinoid/actor-lib/ticker)

(define (raart-render-game get-game-display
                           [width 80] [height 24])
  (raart:matte
   width height
   (raart:place-cursor-after
    (val->raart (or (get-game-display)
                    (raart:blank 0 0)))
    0 0)))

(struct game
  (;; actormap of participants
   actormap
   ;; ticker
   ticker
   ;; environment reset
   reset-env
   ;; display object
   display-obj)
  #:methods gen:word
  [(define (word-tick gw)
     ;; Actors we'll call
     (define reset-env (game-reset-env gw))
     (define ticker (game-ticker gw))
     ;; Transactionally update actors for this tick.
     ;; Either all of this succeeds or nothing succeeds.
     (define (transactional-update!)
       ;; reset the environment
       (reset-env)
       ;; run all objects
       (ticker))
     (actormap-run! (game-actormap gw)
                    transactional-update!)
     gw)
   (define (word-event gw e)
     (match e
       ;; quit
       ["q" #f]
       [_ gw]))
   (define (word-output gw)
     (define (compose-display)
       (raart-render-game (game-display-obj gw)))
     ;; Return the raart to be displayed.
     ;; Note that we use actormap-run rather than -run! because
     ;; there shouldn't be any applicable side effects.
     (actormap-run (game-actormap gw)
                   compose-display))
   (define (word-label gw frame-time)
     "Cauldron time")
   (define (word-fps gw)
     ;; 30?  60???
     ;; probably 30 is something terminals can reliably
     ;; keep up with...
     30.0)
   (define (word-return gw)
     (void))])

(define (new-game)
  (define actormap (make-actormap))
  (define (make-new-game)
    (define display-cell
      (spawn (make-cell)))
    (match-define (list env reset-env)
      (spawn-env-pair))
    (match-define (list ticker-register ticker-tick)
      (spawn-ticker-pair))
    (define (spawn-ticked handler)
      (define ref
        (spawn handler))
      (ticker-register ref)
      ref)
    (spawn-ticked (cauldron spawn-ticked display-cell env))
    (game actormap ticker-tick reset-env display-cell))
  (actormap-run! actormap make-new-game))

(define cauldron-drawing
  "\
.--------------.
 '--   ----  -'
 /            \\
;              ;
:              :
'.            .'
  '----------'")

(define cauldron-raart
  (raart:vappend*
   #:halign 'left
   (map (lambda (l)
          (raart:text l))
        (string-split cauldron-drawing "\n"))))

(define cauldron-width
  (raart:raart-w cauldron-raart))

(define bubble-max-height 10)

(define (make-bubble env bubble-display)
  (define bubble-lifetime
    (random 2 bubble-max-height))
  (define (modify-drift drift)
    (define drift-it
      (random -1 2))
    (max -1 (min 1 (+ drift drift-it))))
  (define raise-delay (random 10 30))
  (define ((lp x y time-till-raise drift) bcom)
    (define time-to-live
      (- bubble-lifetime y))
    (define bubble-shape
      (cond
        ;; big bubbles
        [(>= time-to-live 6)
         #\O]
        ;; medium bubbles
        [(>= time-to-live 2)
         #\o]
        [else #\.]))
    (env 'write bubble-display
         (list x y bubble-shape))
    (define raise-time?
      (eqv? time-till-raise 0))
    (cond
      [raise-time?
       (define new-y
         (add1 y))
       (if (eqv? new-y bubble-lifetime)
           ;; o/~ I tried so hard... and went so far... o/~
           'die
           ;; Time to move and adjust
           (bcom
            (lp (max 0        ; drift, but stay within confines
                     (min (+ x drift)
                          (sub1 cauldron-width))) 
                new-y         ; move up
                raise-delay   ; reset
                (modify-drift drift))))]
      ;; stay the same..
      [else
       (bcom
        (lp x y (sub1 time-till-raise) drift))]))
  (lp (random 2 (- cauldron-width 2))
      0 raise-delay 0))

(define (cauldron spawn-ticked display-cell env)
  (define bubble-display-key
    (env 'new-key #;'bubble-display))
  (define bubble-canvas
    (raart:blank cauldron-width bubble-max-height))
  (define (new-bubble-cooldown)
    (random 15 40))
  (lambda (bcom)
    (let lp ([bubble-cooldown (new-bubble-cooldown)])
      (define bubble-time? (eqv? bubble-cooldown 0))
      (when bubble-time?
        (spawn-ticked (make-bubble env bubble-display-key)))
      (define (do-display)
        (define all-bubbles
          (env 'read bubble-display-key))
        (define bubbled-canvas
          (for/fold ([canvas bubble-canvas])
                    ([bubble-info all-bubbles])
            (match bubble-info
              [(list col row char)
               (raart:place-at canvas
                               (sub1 (- bubble-max-height row))
                               col (raart:char char))])))
        (raart:vappend
         #:halign 'center
         ;; green
         (raart:fg 'green
                   bubbled-canvas)
         ;; yellow
         (raart:fg 'yellow
                   cauldron-raart)))
      (display-cell do-display)
      (bcom
       (lambda _
         (lp (if bubble-time?
                 (new-bubble-cooldown)
                 (sub1 bubble-cooldown))))))))

(define (val->raart val)
  (match val
    [(? raart:raart?) val]
    [(? procedure?) (val)]))

(define (list->raart lst)
  (map (lambda (v)
         (val->raart v))
       lst))

(define (start-game)
  (call-with-chaos
   (raart:make-raart)
   (lambda ()
     (fiat-lux (new-game))))
  (void))

(module+ main
  (start-game))
