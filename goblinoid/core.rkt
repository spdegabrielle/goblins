#lang racket/base

;;; Exports
;;; =======

(require racket/contract)

;; Refs
(provide ref?
         make-near-ref
         near-ref? far-ref? remote-vat-ref?
         near-ref-debug-name near-ref-promise?
         
         far-ref?
         make-far-ref
         far-ref-remote-vat-ref
         far-ref-promise?

         remote-vat-ref?
         make-remote-vat-ref)

;; The making-and-modifying actormap functions
(provide make-actormap
         actormap?
         actormap-ref
         actormap-set!

         snapshot-actormap hasheq->actormap

         transactormap-set! transactormap-ref

         make-transactormap
         transactormap?
         transactormap-parent
         transactormap-delta
         transactormap-merged?
         transactormap-merge!

         actormappable?

         actormappable-ref
         actormappable-set!)

;; The operating-on-actormap main functions
(provide actormap-turn
         actormap-poke!
         actormap-peek
         actormap-turn-message

         actormap-run
         actormap-run*
         actormap-run!

         ;; preventing awkwardness: we don't want actor handlers
         ;; to be actor refs
         (contract-out
          [actormap-spawn
           (->* [any/c
                 (and/c procedure?
                        (not/c ref?))]
                [(or/c #f symbol? string?)]
                (values any/c any/c))]
          [actormap-spawn!
           (->* [any/c (and/c procedure?
                              (not/c ref?))]
                [(or/c #f symbol? string?)]
                any/c)])

         (rename-out [make-next next])
         ;; next? next-return-val next-handler

         call
         (contract-out
          [spawn
           (->* [(and/c procedure?
                        (not/c ref?))]
                [(or/c #f symbol? string?)]
               any/c)])
         <-)

;; Cells

(provide make-cell
         spawn-cell
         cell->read-only
         cell->write-only)

(module+ debug
  (provide actormap-wht))


;;; Imports
;;; =======

(require "message.rkt"
         "hash-contracts.rkt"
         racket/match
         racket/generic
         "ref.rkt")


;;; Refs
;;; ====

(struct ref ())

(struct near-ref ref (debug-name promise?)
  #:constructor-name _make-near-ref
  #:methods gen:custom-write
  [(define (write-proc ref port mode)
     (define str-to-write
       (match (near-ref-debug-name ref)
         [#f "#<near-ref>"]
         ;; TODO: Do we need to do escaping?
         [debug-name (format "#<near-ref ~a>" debug-name)]))
     (write-string str-to-write port))]
  #:property prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args this . args)
     (keyword-apply call kws kw-args this args))))

(define (make-near-ref [debug-name #f]
                       #:promise? [promise? #f])
  (_make-near-ref debug-name promise?))

(struct far-ref ref (remote-vat-ref promise?)
  #:constructor-name _make-far-ref)

(define (make-far-ref remote-vat-ref [promise? #f])
  (_make-far-ref promise?))

;; TODO: Do we add location hints here or somewhere else?
;;   Probably at the vat level and inter-vat level?
;; NOTE: not derived from ref, maybe needs a new name?
(struct remote-vat-ref ()
  #:constructor-name make-remote-vat-ref)


;;; Actormaps and transactormaps
;;; ============================

;; An transactormap is a transactional structure used by the
;; actormap turn system

;; TODO: I don't understand ephemerons and this is probably wrong.

(define-generics actormappable
  (actormappable-ref actormappable key [dflt])
  (actormappable-set! actormappable key val))

(struct actormap (wht) ; weak hash table
  #:methods gen:actormappable
  [(define (actormappable-ref actormap key [dflt #f])
     (actormap-ref actormap key dflt))
   (define (actormappable-set! actormap key [dflt #f])
     (actormap-set! actormap key dflt))])

(define (make-actormap)
  (actormap (make-weak-hasheq)))
(define (actormap-ref actormap key [dflt #f])
  (define val
    (hash-ref (actormap-wht actormap) key #f))
  ;; TODO: we should use the retain argument instead, once that becomes available
  ;;   https://github.com/racket/racket/commit/99feebf070d0dd3e62c697814e0a42508f7995ee
  (or (and val (match (ephemeron-value val key)
                 ;; workaround until retain-v becomes broadly available
                 [(? (lambda (v) (eq? key v)))
                  #f]
                 [result result]))
      dflt))

(define (actormap-set! actormap key val)
  (hash-set! (actormap-wht actormap)
             key (make-ephemeron key val)))

(define (snapshot-actormap actormap)
  (for/fold ([new-hasheq #hasheq()])
            ([(key val) (actormap-wht actormap)])
    (hash-set new-hasheq key val)))

(define (hasheq->actormap ht)
  (define actormap (make-actormap))
  (for ([(key val) ht])
    (actormap-set! actormap key val))
  actormap)

;;; Now the transactional stuff

(struct transactormap (parent delta [merged? #:mutable])
  #:constructor-name _make-transactormap
  #:methods gen:actormappable
  [(define (actormappable-ref transactormap key [dflt #f])
     (transactormap-ref key dflt))
   (define (actormappable-set! transactormap key val)
     (transactormap-set! transactormap key val))])

#;(define actormappable?
  (or/c transactormap? actormap?))

(define/contract (make-transactormap parent)
  (-> actormappable? any/c)
  (_make-transactormap parent (make-hasheq) #f))

(define (transactormap-ref transactormap key [dflt #f])
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-ref on merged transactormap"))
  (or (hash-ref (transactormap-delta transactormap)
                key #f)
      (let ([parent (transactormap-parent transactormap)])
        (match parent
          [(? transactormap?)
           (transactormap-ref parent key #f)]
          [(? actormap?)
           (actormap-ref parent key #f)]))
      dflt))

(define/contract (transactormap-set! transactormap key val)
  (-> transactormap? near-ref? any/c any/c)
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (hash-set! (transactormap-delta transactormap)
             key val)
  (void))

;; Serves two functions:
;;  - to extract the root weak-hasheq
;;  - to merge this transaction on top of the weak-hasheq

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
  (define (do-merge! transactormap)
    (define parent
      (transactormap-parent transactormap))
    (define root-actormap
      (match parent
        [(? transactormap?)
         (do-merge! parent)]
        [(? actormap?)
         parent]))
    (unless (transactormap-merged? transactormap)
      (for ([(key val) (transactormap-delta transactormap)])
        (actormap-set! root-actormap key val))
      (set-transactormap-merged?! transactormap #t))
    root-actormap)
  (do-merge! transactormap)
  (void))

(module+ test
  (require rackunit)

  ;; set up actormap base with beeper and booper
  (define actormap-base (make-actormap))
  (define beeper-ref (make-near-ref 'beeper))
  (define (beeper-proc . args)
    'beep)
  (actormap-set! actormap-base beeper-ref beeper-proc)
  (define booper-ref (make-near-ref 'booper))
  (define (booper-proc . args)
    'boop)
  (actormap-set! actormap-base booper-ref booper-proc)
  (define blepper-ref (make-near-ref 'blepper))
  (define (blepper-proc . args)
    'blep)
  (actormap-set! actormap-base blepper-ref blepper-proc)

  (define tam1
    (make-transactormap actormap-base))
  (define bipper-ref (make-near-ref 'bipper))
  (define (bipper-proc . args)
    'bippity)
  (transactormap-set! tam1 bipper-ref bipper-proc)
  (define (booper-proc2 . args)
    'boop2)
  (transactormap-set! tam1 booper-ref booper-proc2)
  (define (blepper-proc2 . args)
    'blep2)
  (transactormap-set! tam1 blepper-ref blepper-proc2)
  (check-eq? (transactormap-ref tam1 bipper-ref)
             bipper-proc)
  (check-eq? (transactormap-ref tam1 beeper-ref)
             beeper-proc)
  (check-eq? (transactormap-ref tam1 booper-ref)
             booper-proc2)
  (check-eq? (transactormap-ref tam1 blepper-ref)
             blepper-proc2)
  (check-eq? (actormap-ref actormap-base booper-ref #f)
             booper-proc)
  (check-false (transactormap-merged? tam1))

  (define tam2
    (make-transactormap tam1))

  (define boppiter-ref (make-near-ref 'boppiter))
  (define (boppiter-proc . args)
    'boppitty)
  (transactormap-set! tam2 boppiter-ref boppiter-proc)
  (define (booper-proc3 . args)
    'boop3)
  (transactormap-set! tam2 booper-ref booper-proc3)

  (check-eq? (transactormap-ref tam2 beeper-ref)
             beeper-proc)
  (check-eq? (transactormap-ref tam2 booper-ref)
             booper-proc3)
  (check-eq? (transactormap-ref tam2 bipper-ref)
             bipper-proc)
  (check-eq? (transactormap-ref tam2 boppiter-ref)
             boppiter-proc)
  (check-eq? (transactormap-ref tam2 blepper-ref)
             blepper-proc2)
  (check-eq? (actormap-ref actormap-base booper-ref #f)
             booper-proc)
  (check-eq? (actormap-ref actormap-base boppiter-ref #f)
             #f)
  (check-false (transactormap-merged? tam2))

  (transactormap-merge! tam2)
  (check-true (transactormap-merged? tam2))
  (check-true (transactormap-merged? tam1))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam2 beeper-ref)))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam1 beeper-ref)))
  (check-exn any/c
             (lambda ()
               (transactormap-set! tam1 beeper-ref
                                   (lambda _ 'whatever))))

  (check-eq? (actormap-ref actormap-base beeper-ref)
             beeper-proc)
  (check-eq? (actormap-ref actormap-base booper-ref)
             booper-proc3)
  (check-eq? (actormap-ref actormap-base bipper-ref)
             bipper-proc)
  (check-eq? (actormap-ref actormap-base boppiter-ref)
             boppiter-proc)
  (check-eq? (actormap-ref actormap-base blepper-ref)
             blepper-proc2))

;;; Syscaller internals
;;; ===================
;;;
;;; NEVER export these.

(define current-syscaller
  (make-parameter #f))

(define (get-syscaller-or-die)
  (define sys (current-syscaller))
  (unless sys
    (error "No current syscaller"))
  sys)

(define (call-with-fresh-syscaller actormap proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller actormap))
  (begin0 (parameterize ([current-syscaller sys])
            (proc sys get-sys-internals))
    (close-up!)))

(define (fresh-syscaller prev-actormap)
  (define actormap
    (make-transactormap prev-actormap))
  (define to-local '())
  (define to-remote '())

  (define closed? #f)

  (define this-syscaller
    (make-keyword-procedure
     (lambda (kws kw-args method-id . args)
       (when closed?
         (error "Sorry, this syscaller is closed for business!"))
       (define method
         (match method-id
           ['call call]
           ['spawn _spawn]
           ['<- <-]
           [_ (error "invalid syscaller method")]))
       (keyword-apply method kws kw-args args))))

  ;; call actor's handler
  (define call
    (make-keyword-procedure
     (lambda (kws kw-args to-ref . args)
       (define actor-handler
         (transactormap-ref actormap to-ref #f))
       (unless actor-handler
         (error "Can't send message; no actor with this id"))
       (define result
         (keyword-apply actor-handler kws kw-args args))
       
       (define-values (return-val new-handler)
         (match result
           [(? next?)
            (values (next-return-val result)
                    (next-handler result))]
           [_ (values result #f)]))

       ;; if a new handler for this actor was specified,
       ;; let's replace it
       (when new-handler
         (transactormap-set! actormap to-ref new-handler))

       return-val)))

  ;; spawn a new actor
  (define (_spawn actor-handler
                  [debug-name (object-name actor-handler)])
    (actormap-spawn! actormap actor-handler debug-name))

  (define <-
    (make-keyword-procedure
     (lambda (kws kw-args actor-ref . args)
       (define new-message
         (message actor-ref kws kw-args args))
       (match actor-ref
         [(? near-ref?)
          (set! to-local (cons new-message to-local))]
         [(? far-ref?)
          (set! to-remote (cons new-message to-remote))]))))

  (define (get-internals)
    (list actormap to-local to-remote))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))


;;; setting up next handler
;;; =======================

(struct next (handler return-val)
  #:constructor-name _make-next)
(define/contract (make-next handler [return-val (void)])
  (->* [(and/c procedure? (not/c ref?))]
       [(not/c next?)]
       any/c)
  (_make-next handler return-val))

(module+ extra-next
  (provide next next? next-handler next-return-val))


;;; syscall external functions
;;; ==========================

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to-ref . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<- to-ref args))))

(define call
  (make-keyword-procedure
   (lambda (kws kw-args to-ref . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args 'call to-ref args))))

(define (spawn actor-handler
               [debug-name (object-name actor-handler)])
  (define sys (get-syscaller-or-die))
  (sys 'spawn actor-handler debug-name))


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-ref kws kw-args args)
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (keyword-apply sys kws kw-args 'call to-ref args))
     (apply values result-val
            (get-sys-internals)))))

(define actormap-turn
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (actormap-turn* actormap to-ref kws kw-args args))))

;; Note that this does nothing with the messages.
(define actormap-poke!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     (transactormap-merge! transactormap)
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-peek
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     returned-val)))

(define (actormap-turn-message actormap message)
  (define to (message-to message))
  (unless (near-ref? to)
    (error "Can only perform a turn on a message to local actors"))
  (actormap-turn* actormap to
                  (message-kws message)
                  (message-kw-vals message)
                  (message-args message)))

;; The following two are utilities for when you want to check
;; or bootstrap something within an actormap

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val new-actormap2)
    (actormap-run* actormap thunk))
  returned-val)

;; like actormap-run but also returns the new actormap
(define (actormap-run* actormap thunk)
  (define-values (actor-ref new-actormap)
    (actormap-spawn actormap thunk))
  (define-values (returned-val new-actormap2 _tl _tr)
    (actormap-turn* new-actormap actor-ref '() '() '()))
  (values returned-val new-actormap2))

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define (actormap-run! actormap thunk)
  (define actor-ref
    (actormap-spawn! actormap thunk))
  (actormap-poke! actormap actor-ref))


;; Spawning
;; ========

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-handler
                        [debug-name (object-name actor-handler)])
  (define actor-ref
    (make-near-ref debug-name))
  (define new-actormap
    (make-transactormap actormap))
  (transactormap-set! new-actormap actor-ref actor-handler)
  (values actor-ref new-actormap))

(define (actormap-spawn! actormap actor-handler
                         [debug-name (object-name actor-handler)])
  (define actor-ref
    (make-near-ref debug-name))
  (actormappable-set! actormap actor-ref actor-handler)
  actor-ref)

(module+ test
  (require rackunit
           racket/contract)
  (define am (make-actormap))

  (define ((counter n))
    (make-next (counter (add1 n))
               n))

  ;; can actors update themselves?
  (define ctr-ref
    (actormap-spawn! am (counter 1) 'ctr))
  (define-values (turned-val1 am+ctr1 _to-local _to-remote)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1 1)
  (define-values (turned-val2 am+ctr2 _to-local2 _to-remote2)
    (actormap-turn am+ctr1 ctr-ref))
  (check-eqv? turned-val2 2)

  ;; transaction shouldn't be applied yet
  (define-values (turned-val1-again
                  am+ctr1-again
                  _to-local-again _to-remote-again)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1-again 1)

  ;; but now it should be
  (transactormap-merge! am+ctr2)
  (define-values (turned-val3 am+ctr3 _to-local3 _to-remote3)
    ;; observe that we're turning using the "original"
    ;; actormap though!  It should have committed.
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val3 3)

  (define (friend-spawner friend-name)
    (define ((a-friend [called-times 0]))
      (define new-called-times
        (add1 called-times))
      (make-next (a-friend new-called-times)
                 (format "Hello!  My name is ~a and I've been called ~a times!"
                         friend-name new-called-times)))
    (spawn (a-friend) 'friend))
  (define fr-spwn (actormap-spawn! am friend-spawner))
  (define joe (actormap-poke! am fr-spwn 'joe))
  (check-equal?
   (actormap-peek am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-poke! am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-poke! am joe)
   "Hello!  My name is joe and I've been called 2 times!")

  (define-values (noncommital-ref noncommital-am)
    (actormap-spawn am (lambda () 'noncommital)))
  (check-eq?
   (actormap-peek noncommital-am noncommital-ref)
   'noncommital)
  (check-exn
   any/c
   (lambda () (actormap-peek am noncommital-ref)))

  (define who-ya-gonna-call
    #f)
  (define (make-and-call-friend)
    (define new-friend-spawner
      (spawn friend-spawner))
    (define friend
      (call new-friend-spawner 'brian))
    (set! who-ya-gonna-call friend)
    (call friend))
  (check-equal?
   (actormap-run am make-and-call-friend)
   "Hello!  My name is brian and I've been called 1 times!")
  (check-exn
   any/c
   (lambda () (actormap-peek am who-ya-gonna-call)))
  ;; now run it with commitment
  (check-equal?
   (actormap-run! am make-and-call-friend)
   "Hello!  My name is brian and I've been called 1 times!")
  (check-equal?
   (actormap-peek noncommital-am who-ya-gonna-call)
   "Hello!  My name is brian and I've been called 2 times!"))

;;; Cells
;;; =====

;; A simple turn-mutable cell

(define (make-cell [val #f])
  (case-lambda
    [() val]
    [(new-val)
     (make-next (make-cell new-val))]))

(define (spawn-cell [val #f])
  (spawn (make-cell val)))

(define (cell->read-only cell)
  (spawn (lambda () (cell))))

(define (cell->write-only cell)
  (spawn (lambda (new-val) (cell new-val))))

(module+ test
  (define a-cell
    (actormap-spawn! am (make-cell)))
  (test-eq?
   "cell without default value and unset is #f"
   (actormap-peek am a-cell)
   #f)
  (actormap-poke! am a-cell 'foo)
  (test-eq?
   "cell after being set retains value"
   (actormap-peek am a-cell)
   'foo)
  
  (test-eq?
   "cell default values"
   (actormap-peek am (actormap-spawn! am (make-cell 'hello)))
   'hello))
