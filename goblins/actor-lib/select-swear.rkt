#lang racket

(require "../core.rkt")

(provide select-$/<-p
         run-$/<-p)

;; A helper to select $ or <-p.
;; Combined they look like a cartoon character swearing.
;; Probably a better name is warranted.
(define (select-$/<-p to-refr)
  (if (near-refr? to-refr)
      $ <-p))

(define run-$/<-p
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (keyword-apply (select-$/<-p to-refr)
                    kws kw-args to-refr args))))

(module+ test
  (require "../vat.rkt"
           rackunit)
  (define vat-a (make-vat))
  (define vat-b (make-vat))
  (define ((^simple-robot bcom))
    'beep-boop)
  (define robot-a
    (vat-a 'spawn ^simple-robot))
  (define robot-b
    (vat-b 'spawn ^simple-robot))
  
  (define ((^swear-selector bcom) to)
    (select-$/<-p to))

  (define swear-selector-a
    (vat-a 'spawn ^swear-selector))

  (test-eq?
   "select-$/<-p to object on same vat gets $"
   (vat-a 'call swear-selector-a robot-a)
   $)

  (test-eq?
   "select-$/<-p to object on remote vat gets <-p"
   (vat-a 'call swear-selector-a robot-b)
   <-p))
