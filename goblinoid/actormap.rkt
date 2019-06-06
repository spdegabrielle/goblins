#lang racket/base

;; An transactormap is a transactional structure used by the
;; actormap turn system

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

(module+ debug
  (provide actormap-wht))

(require racket/contract
         racket/match
         racket/generic
         "ref.rkt"
         "hash-contracts.rkt")

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
