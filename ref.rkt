#lang racket

(provide (struct-out near-ref)
         (struct-out far-ref)
         (struct-out remote-vat-ref))

(struct near-ref (debug-name)
  #:transparent
  #:constructor-name make-near-ref)

(struct far-ref (remote-vat-ref)
  #:transparent
  #:constructor-name make-far-ref)

;; TODO: Do we add location hints here or somewhere else?
;;   Probably at the vat level and inter-vat level?
(struct remote-vat-ref ())
