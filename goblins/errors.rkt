#lang racket/base

(provide (struct-out exn:fail:mystery)
         make-mystery-fail)

(struct exn:fail:mystery exn ()
  #:transparent)

(define (make-mystery-fail)
  (exn:fail:mystery "Who knows?" (continuation-marks #f)))
