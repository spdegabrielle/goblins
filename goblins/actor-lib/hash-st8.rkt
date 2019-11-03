#lang racket/base

(provide ^st8 ^loose-st8)

(require "../core.rkt"
         racket/match)

(define (raise-invalid-st8-pairs val)
  (error 'invalid-st8-pairs "Expected key-value pairs, got: ~a" val))

(define (make-^st8 loose?)
  (make-keyword-procedure
   (lambda (kws kw-args bcom . args)
     (define ht-pre-kws
       (match args
         ['() '#hasheq()]
         [(list ht) ht]))
     (define initial-ht
       (for/fold ([ht ht-pre-kws])
                 ([kw kws]
                  [kw-arg kw-args])
         (hash-set ht (string->symbol (keyword->string kw)) kw-arg)))
     (define (^next bcom ht)
       (match-lambda*
         ;; get
         [(list key)
          (hash-ref ht key)]
         ;; set (possibly multiple)
         [kv-pairs
          (define new-ht
            (let lp ([kv-pairs kv-pairs])
              (match kv-pairs
                ['() ht]
                [(list (? symbol? key) val rest-pairs ...)
                 (unless (or loose?
                             (hash-has-key? ht key))
                   (error 'st8-undefined-key
                          "Key not defined in strict st8 object: ~a" key))
                 (hash-set (lp rest-pairs)
                           key val)]
                [_
                 (raise-invalid-st8-pairs kv-pairs)])))
          (bcom ^next new-ht)]))

     (^next bcom initial-ht))))

(define ^st8
  (procedure-rename (make-^st8 #f)
                    '^st8))

(define ^loose-st8
  (procedure-rename (make-^st8 #t)
                    '^loose-st8))

(module+ test
  (require rackunit
           racket/contract)
  
  (define am (make-actormap))
  (define st8
    (actormap-spawn! am ^st8
                     #:foo 'first-foo
                     #:bar 'first-bar))

  (check-equal?
   (actormap-peek am st8 'foo)
   'first-foo)
  (check-equal?
   (actormap-peek am st8 'bar)
   'first-bar)
  (check-exn
   any/c
   (lambda ()
     (actormap-peek am st8 'baz)))

  (actormap-poke! am st8 'foo 'second-foo)
  (check-equal?
   (actormap-peek am st8 'foo)
   'second-foo)
  (check-equal?
   (actormap-peek am st8 'bar)
   'first-bar)

  (actormap-poke! am st8
                  'foo 'final-foo
                  'bar 'final-bar)
  (check-equal?
   (actormap-peek am st8 'foo)
   'final-foo)
  (check-equal?
   (actormap-peek am st8 'bar)
   'final-bar))
