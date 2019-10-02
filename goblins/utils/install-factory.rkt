#lang racket

(provide install-default-factories!)

(require crypto crypto/libcrypto)

(define (install-default-factories!)
  (when (null? (crypto-factories))
    (crypto-factories (list libcrypto-factory))))
