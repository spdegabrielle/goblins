#lang racket/base

(provide make-env)

(require "../utils/simple-sealers.rkt"
         "../main.rkt"
         "facet.rkt"
         racket/match)

(define (rw->read-key rw-key)
  (match rw-key
    [(list 'rw-key read-key write-key)
     (list 'read-key read-key)]))

(define (rw->write-key rw-key)
  (match rw-key
    [(list 'rw-key read-key write-key)
     (list 'write-key write-key)]))

(define (rw-key? val)
  (match val
    [(list 'rw-key _)
     #t]
    [_ #f]))

(define (read-key? val)
  (match val
    [(list 'read-key _)
     #t]
    [_ #f]))

(define (write-key? val)
  (match val
    [(list 'write-key _)
     #t]
    [_ #f]))

(define (make-env)
  (define-values (seal unseal branded?)
    (make-sealer-triplet))

  (define (extract/verify-read-id key)
    (match key
      [(list 'rw-key sealed-read sealed-write)
       (match (unseal sealed-read)
         [(list 'read id)
          id])]
      [(list 'read-key sealed-read)
       (match (unseal sealed-read)
         [(list 'read id)
          id])]))

  (define (extract/verify-write-id key)
    (match key
      [(list 'rw-key sealed-read sealed-write)
       (match (unseal sealed-write)
         [(list 'write id)
          id])]
      [(list 'write-key sealed-write)
       (match (unseal sealed-write)
         [(list 'write id)
          id])]))

  (define (env ht)
    (match-lambda*
      [(list 'new-key)
       ;; unique by eq?
       (define id
         (cons 'tick 'key))
       (define read-key
         (seal `(read ,id)))
       (define write-key
         (seal `(write ,id)))
       (define rw-key
         `(rw-key ,read-key ,write-key))
       rw-key]
      [(list 'read readable-key)
       (define id
         (extract/verify-read-id readable-key))
       (hash-ref ht id '())]
      [(list 'write writeable-key val)
       (define id
         (extract/verify-write-id writeable-key))
       (define updated-ht
         (hash-set ht id
                   (cons val (hash-ref ht id '()))))
       (define new-env
         (env updated-ht))
       (values (void) new-env)]
      [(list 'reset)
       (define new-env
         (env #hasheq()))
       (values (void) new-env)]))
  
  (env #hasheq()))

(define (spawn-env-pair)
  (define this-env
    (spawn (make-env)))
  (define rw-facet
    (spawn (facet this-env 'new-key 'read 'write)))
  (define reset-facet
    (spawn (facet this-env 'reset)))
  (list rw-facet reset-facet))

(module+ test
  (require rackunit
           racket/contract)
  (define am (make-actormap))
  (match-define (list rw-facet reset-facet)
    (actormap-run! am spawn-env-pair))
  (define foo-rw-key
    (actormap-poke! am rw-facet 'new-key))
  (test-equal?
   "empty read to foo"
   (actormap-poke! am rw-facet 'read foo-rw-key)
   '())
  (actormap-poke! am rw-facet 'write foo-rw-key 'foo-1)
  (test-equal?
   "one write to foo"
   (actormap-poke! am rw-facet 'read foo-rw-key)
   '(foo-1))
  (define foo-read-key
    (rw->read-key foo-rw-key))
  (define foo-write-key
    (rw->write-key foo-rw-key))
  (test-exn
   "write-keys can't read"
   any/c
   (lambda ()
     (actormap-poke! am rw-facet 'read foo-write-key)))
  (test-exn
   "read-keys can't write"
   any/c
   (lambda ()
     (actormap-poke! am rw-facet 'write foo-read-key 'uhoh)))
  (actormap-poke! am rw-facet 'write foo-write-key 'foo-2)
  (test-equal?
   "writes to foo using separate read/write keys"
   (actormap-poke! am rw-facet 'read foo-read-key)
   '(foo-2 foo-1))
  (test-exn
   "Can't reset through rw-facet"
   any/c
   (lambda ()
     (actormap-poke! am rw-facet 'reset)))
  (actormap-poke! am reset-facet 'reset)
  (test-equal?
   "read after reset is empty"
   (actormap-poke! am rw-facet 'read foo-read-key)
   '()))
