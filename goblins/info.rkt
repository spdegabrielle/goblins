#lang info

(define deps '("base" "crypto"))
(define build-deps '("rackunit-lib" "scribble-lib" "sandbox-lib"))
(define pkg-desc
  "A transactional, distributed actor model environment")
(define version "0.6")
(define pkg-authors '("cwebber"))
(define scribblings '(("scribblings/goblins.scrbl" (multi-page))))
