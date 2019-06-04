#lang racket

(require racket/generic)

(provide gen:actormap
         actormap-set actormap-ref
         immutable-hasheq?

         make-weak-actormap
         weak-actormap?
         weak-actormap-parent
         weak-actormap-delta
         weak-actormap-merged?
         weak-actormap-merge!)

(define (immutable-hasheq? val)
  (and (hash? val)
       (hash-eq? val)
       (immutable? val)))

(define-generics actormap
  (actormap-set actormap key val)
  (actormap-ref actormap key [dflt])
  #:defaults
  ([immutable-hasheq?
    (define actormap-set hash-set)
    (define actormap-ref hash-ref)]))

;; for performance
(struct weak-actormap (parent delta [merged? #:mutable])
  #:constructor-name _make-weak-actormap)

(define/contract (make-weak-actormap parent)
  (-> (or/c immutable-hasheq? weak-actormap?) any/c)
  (_make-weak-actormap parent #hasheq() #f))

;; Probably not threadsafe, and probably doesn't matter
(define weak-hasheq/c
  (and/c hash? hash-eq? hash-weak?))

(define (weak-actormap-merge! weak-actormap)
  (define (do-merge! weak-actormap)
    (define parent
      (weak-actormap-parent))
    (define root-weakmap
      (match parent
        [(? weak-actormap?)
         (do-merge! parent)]
        [(? weak-hasheq/c)
         parent]))
    (if (weak-actormap-merged? weak-actormap)
        root-weakmap
        (begin
          (for ([(key val) (weak-actormap-delta weak-actormap)])
            (hash-set! root-weakmap key val))
          (set-weak-actormap-merged?! weak-actormap #t))))
  (do-merge! weak-actormap)
  (void))
