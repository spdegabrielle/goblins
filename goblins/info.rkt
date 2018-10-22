#lang info

(define name "goblins")
;; Some of these may just be development deps?
(define deps '("base"))
(define build-deps
  '("scribble-lib"
    "rackunit-lib"
    "sandbox-lib"))
(define pkg-desc "Actor library")
(define version "0.0")
(define pkg-authors '("cwebber"))
(define scribblings
  '(("scribblings/goblins.scrbl")))
