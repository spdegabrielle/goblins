#lang info

(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib"))
(define pkg-desc
  "Transactional actor system inspired by the E programming language")
(define version "0.0")
(define pkg-authors '("cwebber"))
#;(define raco-commands '(("crystal" (submod crystal/client main)
                                   "Run crystal client"
                                   #f)
                        ("crystal-server"
                         (submod crystal/crystal-server main)
                         "Run a crystal registry as a server"
                         #f)))
#;(define scribblings '(("scribblings/crystal.scrbl")))