#lang racket/base

(require racket/contract
         racket/set
         racket/match
         "../core.rkt")

(provide ^facet)

(define (^facet bcom wrap-me . methods)
  (define method-set (apply seteq methods))
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (match args
       [(list (? symbol? method) args ...)
        (unless (set-member? method-set method)
          (error (format "Access to method ~a denied" method)))
        (keyword-apply $/<-p kws kw-args wrap-me method args)]
       [_ "Requires symbol-based method dispatch"]))))

(module+ test
  (require rackunit
           "../core.rkt")
  (define am (make-actormap))
  (define (^wizard bcom)
    (match-lambda*
     [(list 'magic-missile level)
      (format "Casts magic missile level ~a!"
              level)]
     [(list 'flame-tongue level)
      (format "Casts flame tongue level ~a!"
              level)]
     [(list 'world-ender level)
      (format "Casts world ender level ~a!"
              level)]))
  (define all-powerful-wizard
    (actormap-spawn! am ^wizard))
  (define faceted-wizard
    (actormap-spawn! am ^facet all-powerful-wizard
                     'magic-missile
                     'flame-tongue))
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
