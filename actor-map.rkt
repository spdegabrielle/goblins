#lang racket/base

;; An transactormap is a transactional structure used by the
;; actormap turn system

(provide transactormap-set! transactormap-ref

         make-transactormap
         transactormap?
         transactormap-parent
         transactormap-delta
         transactormap-merged?
         transactormap-merge!

         snapshot-hasheq hasheq->weak-hasheq

         actormap/c)

(require racket/contract
         racket/match
         "ref.rkt"
         "hash-contracts.rkt")

(struct transactormap (parent delta [merged? #:mutable])
  #:constructor-name _make-transactormap)

(define/contract (make-transactormap parent)
  (-> (or/c transactormap? weak-hasheq/c) any/c)
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
          [(? hash?)
           (hash-ref parent key #f)]))
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
    (define root-weakmap
      (match parent
        [(? transactormap?)
         (do-merge! parent)]
        [(? weak-hasheq/c)
         parent]))
    (unless (transactormap-merged? transactormap)
      (for ([(key val) (transactormap-delta transactormap)])
        (hash-set! root-weakmap key val))
      (set-transactormap-merged?! transactormap #t))
    root-weakmap)
  (do-merge! transactormap)
  (void))

(define (snapshot-hasheq ht)
  (for/fold ([new-hasheq #hasheq()])
            ([(key val) ht])
    (hash-set new-hasheq key val)))

(define (hasheq->weak-hasheq ht)
  (define weak-hasheq (make-hasheq))
  (for ([(key val) ht])
    (hash-set! ht key val))
  (void))

(define actormap/c
  (or/c transactormap? weak-hasheq/c))

(module+ test
  (require rackunit)

  ;; set up actormap base with beeper and booper
  (define actormap-base (make-weak-hasheq))
  (define beeper-ref (make-near-ref 'beeper))
  (define (beeper-proc . args)
    'beep)
  (hash-set! actormap-base beeper-ref beeper-proc)
  (define booper-ref (make-near-ref 'booper))
  (define (booper-proc . args)
    'boop)
  (hash-set! actormap-base booper-ref booper-proc)
  (define blepper-ref (make-near-ref 'blepper))
  (define (blepper-proc . args)
    'blep)
  (hash-set! actormap-base blepper-ref blepper-proc)

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
  (check-eq? (hash-ref actormap-base booper-ref #f)
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
  (check-eq? (hash-ref actormap-base booper-ref #f)
             booper-proc)
  (check-eq? (hash-ref actormap-base boppiter-ref #f)
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

  (check-eq? (hash-ref actormap-base beeper-ref)
             beeper-proc)
  (check-eq? (hash-ref actormap-base booper-ref)
             booper-proc3)
  (check-eq? (hash-ref actormap-base bipper-ref)
             bipper-proc)
  (check-eq? (hash-ref actormap-base boppiter-ref)
             boppiter-proc)
  (check-eq? (hash-ref actormap-base blepper-ref)
             blepper-proc2))
