#lang racket/base

(require racket/contract
         racket/set
         racket/match
         "../core.rkt")

(provide facet facet*)

(define/contract (facet* wrap-me methods)
  (-> refr? (set/c symbol?
                   #:cmp 'eq
                   #:kind 'immutable)
      any/c)
  (define facet
    (make-keyword-procedure
     (lambda (kws kw-args become . args)
       (match args
         [(list (? symbol? method) args ...)
          (unless (set-member? methods method)
            (error (format "Access to method ~a denied" method)))
          (apply call wrap-me method args)]
         [_ "Requires symbol-based method dispatch"]))))
  facet)

(define (facet wrap-me . methods)
  (facet* wrap-me (apply seteq methods)))

(module+ test
  (require rackunit
           "../core.rkt"
           "match-methods.rkt")
  (define am (make-actormap))
  (define all-powerful-wizard
    (actormap-spawn! am (match-methods
                         become
                         [(magic-missile level)
                          (format "Casts magic missile level ~a!"
                                  level)]
                         [(flame-tongue level)
                          (format "Casts flame tongue level ~a!"
                                  level)]
                         [(world-ender level)
                          (format "Casts world ender level ~a!"
                                  level)])))
  (define faceted-wizard
    (actormap-spawn! am
                     (facet all-powerful-wizard
                            'magic-missile
                            'flame-tongue)))
  (check-equal?
   (actormap-peek am faceted-wizard 'magic-missile 2)
   "Casts magic missile level 2!")
  (check-equal?
   (actormap-peek am faceted-wizard 'flame-tongue 3)
   "Casts flame tongue level 3!")
  (check-exn
   any/c
   (lambda ()
     (actormap-peek am faceted-wizard 'world-ender 99))))
