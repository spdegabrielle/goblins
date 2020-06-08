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
       (let lp ([args args])
         (match args
           ['() '#hasheq()]
           [(list (? hash-eq? ht)) ht]
           [(list (? symbol? key) val rest-args ...)
            (hash-set (lp rest-args)
                      key args)])))
     (define initial-ht
       (for/fold ([ht ht-pre-kws])
                 ([kw (in-list kws)]
                  [kw-arg (in-list kw-args)])
         (hash-set ht (string->symbol (keyword->string kw)) kw-arg)))
     (define (next ht)
       (make-keyword-procedure
        (lambda (kws kw-args . args)
          (match args
            ;; get
            [(list key)
             (hash-ref ht key)]
            ;; set (possibly multiple)
            [kv-pairs
             (define (maybe-ensure-ht-has-key key)
               (unless (or loose?
                           (hash-has-key? ht key))
                 (error 'st8-undefined-key
                        "Key not defined in strict st8 object: ~a" key)))
             (define kws-kwargs-ht
               (for/fold ([ht ht])
                         ([kw kws]
                          [kw-arg kw-args])
                 (define key
                   (string->symbol (keyword->string kw)))
                 (maybe-ensure-ht-has-key key)
                 (hash-set ht key kw-arg)))
             (define new-ht
               (let lp ([kv-pairs kv-pairs])
                 (match kv-pairs
                   ['() kws-kwargs-ht]
                   [(list (? symbol? key) val rest-pairs ...)
                    (maybe-ensure-ht-has-key key)
                    (hash-set (lp rest-pairs)
                              key val)]
                   [_
                    (raise-invalid-st8-pairs kv-pairs)])))
             (bcom (next new-ht))]))))

     (next initial-ht))))

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
                  'foo 'third-foo
                  'bar 'third-bar)
  (check-equal?
   (actormap-peek am st8 'foo)
   'third-foo)
  (check-equal?
   (actormap-peek am st8 'bar)
   'third-bar)
  (actormap-poke! am st8
                  #:foo 'fourth-foo
                  #:bar 'fourth-bar)
  (check-equal?
   (actormap-peek am st8 'foo)
   'fourth-foo)
  (check-equal?
   (actormap-peek am st8 'bar)
   'fourth-bar))
