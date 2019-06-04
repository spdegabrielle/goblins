#lang racket/base

(provide weak-hasheq/c
         immutable-hasheq?)

(require racket/contract)

(define weak-hasheq/c
  (and/c hash? hash-eq? hash-weak?))

(define (immutable-hasheq? val)
  (and (hash? val)
       (hash-eq? val)
       (immutable? val)))
