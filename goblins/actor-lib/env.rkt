#lang racket/base

;; An environment that multiple entities can read/write to,
;; inspired by DOS/Win and later DOS/Hurd

(provide ^env
         spawn-env-pair
         rw->read-key
         rw->write-key
         rw-key?
         read-key?
         write-key?)

(require "../utils/simple-sealers.rkt"
         "../core.rkt"
         "facet.rkt"
         "mactor.rkt"
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

(define (^env bcom)
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

  (define (^next-env bcom ht)
    (mactor
      [(new-key [key-name 'some-key])
       ;; unique by eq?
       (define id
         (list key-name))
       (define read-key
         (seal `(read ,id)))
       (define write-key
         (seal `(write ,id)))
       (define rw-key
         `(rw-key ,read-key ,write-key))
       rw-key]
      [(read readable-key)
       (define id
         (extract/verify-read-id readable-key))
       (hash-ref ht id '())]
      [(write writeable-key val)
       (define id
         (extract/verify-write-id writeable-key))
       (define updated-ht
         (hash-set ht id
                   (cons val (hash-ref ht id '()))))
       (bcom ^next-env updated-ht)]
      [(reset)
       (bcom ^next-env #hasheq())]))
  
  (^next-env bcom #hasheq()))

(define (spawn-env-pair)
  (define this-env
    (spawn ^env))
  (define rw-facet
    (spawn (procedure-rename ^facet '^rw-facet)
           this-env 'new-key 'read 'write))
  (define ((^reset-facet bcom))
    (call this-env 'reset))
  (define reset-facet
    (spawn ^reset-facet))
  (list rw-facet reset-facet))

(module+ test
  (require rackunit
           racket/contract)
  (define am (make-actormap))
  (match-define (list env reset-env)
    (actormap-run! am spawn-env-pair))
  (define foo-rw-key
    (actormap-poke! am env 'new-key))
  (test-equal?
   "empty read to foo"
   (actormap-poke! am env 'read foo-rw-key)
   '())
  (actormap-poke! am env 'write foo-rw-key 'foo-1)
  (test-equal?
   "one write to foo"
   (actormap-poke! am env 'read foo-rw-key)
   '(foo-1))
  (define foo-read-key
    (rw->read-key foo-rw-key))
  (define foo-write-key
    (rw->write-key foo-rw-key))
  (test-exn
   "write-keys can't read"
   any/c
   (lambda ()
     (actormap-poke! am env 'read foo-write-key)))
  (test-exn
   "read-keys can't write"
   any/c
   (lambda ()
     (actormap-poke! am env 'write foo-read-key 'uhoh)))
  (actormap-poke! am env 'write foo-write-key 'foo-2)
  (test-equal?
   "writes to foo using separate read/write keys"
   (actormap-poke! am env 'read foo-read-key)
   '(foo-2 foo-1))
  (test-exn
   "Can't reset through env's rw-facet"
   any/c
   (lambda ()
     (actormap-poke! am env 'reset)))
  (actormap-poke! am reset-env)
  (test-equal?
   "read after reset is empty"
   (actormap-poke! am env 'read foo-read-key)
   '()))
