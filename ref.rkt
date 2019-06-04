#lang racket/base

(provide (struct-out near-ref)
         (struct-out far-ref)
         (struct-out remote-vat-ref))

(require racket/match)

(struct near-ref (debug-name)
  #:constructor-name make-near-ref
  #:methods gen:custom-write
  [(define (write-proc ref port mode)
     (define str-to-write
       (match (near-ref-debug-name ref)
         [#f "#<near-ref>"]
         ;; TODO: Do we need to do escaping?
         [debug-name (format "#<near-ref ~a>" debug-name)]))
     (write-string str-to-write port))])

(struct far-ref (remote-vat-ref)
  #:constructor-name make-far-ref)

;; TODO: Do we add location hints here or somewhere else?
;;   Probably at the vat level and inter-vat level?
(struct remote-vat-ref ()
  #:constructor-name make-remote-vat-ref)
