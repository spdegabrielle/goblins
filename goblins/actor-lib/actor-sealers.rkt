#lang racket/base

(provide spawn-sealer-triplet
         spawn-sealer-triplet-list)

(require "../core.rkt"
         "../utils/simple-sealers.rkt"
         racket/match
         racket/contract)

(define (spawn-sealer-triplet [name #f])
  (define-values (seal unseal sealed?)
    (make-sealer-triplet name))
  (define (rename-with-name constructor prefix)
    (procedure-rename constructor
                      (match name
                        [(or (? string?) (? symbol?))
                         (string->symbol (format "~a: ~a" prefix name))]
                        [#f prefix])))

  (define (_^sealed _bcom value)
    (define sealed-by-struct
      (seal value))
    ;; the actual handler is just something that returns the struct-sealed
    ;; version of things
    (lambda () sealed-by-struct))

  (define ((_^sealer _bcom) value)
    (spawn ^sealed value))
  (define (_^unsealer _bcom)
    (define/contract (unseal-it sealed)
      (-> near-refr? any/c)
      (unseal ($ sealed)))
    unseal-it)
  (define ((_^sealed? _bcom) sealed)
    (sealed? ($ sealed)))

  (define ^sealed (rename-with-name _^sealed 'sealed))
  (define ^sealer (rename-with-name _^sealer 'sealer))
  (define ^unsealer (rename-with-name _^unsealer 'unsealer))
  (define ^sealed? (rename-with-name _^sealed? 'sealed?))

  (values (spawn ^sealer)
          (spawn ^unsealer)
          (spawn ^sealed?)))

(define (spawn-sealer-triplet-list [name #f])
  (call-with-values
   (lambda ()
     (spawn-sealer-triplet name))
   list))

(module+ test
  (require rackunit)
  (define am
    (make-actormap))

  (match-define (list alice-sealer alice-unsealer alice-sealed?)
    (actormap-run! am
                   (lambda ()
                     (spawn-sealer-triplet-list "Alice"))))

  (match-define (list bob-sealer bob-unsealer bob-sealed?)
    (actormap-run! am
                   (lambda ()
                     (spawn-sealer-triplet-list "Bob"))))

  (define alice-sealed-lunch
    (actormap-poke! am alice-sealer 'chickpea-salad))
  (define bob-sealed-lunch
    (actormap-poke! am bob-sealer 'bbq-lentils))

  (test-equal?
   "Alice can unseal her sealed lunch"
   (actormap-peek am alice-unsealer alice-sealed-lunch)
   'chickpea-salad)

  (test-exn
   "Alice cannot unseal Bob's sealed lunch"
   exn:fail:contract?
   (lambda ()
     (actormap-peek am alice-unsealer bob-sealed-lunch)))
  
  (test-true
   "Alices lunch trademark recognizes Alice's lunch"
   (actormap-peek am alice-sealed? alice-sealed-lunch))

  (test-false
   "Alices lunch trademark does not recognize Bob's lunch"
   (actormap-peek am alice-sealed? bob-sealed-lunch)))
