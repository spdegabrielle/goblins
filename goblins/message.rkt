#lang racket/base

(provide (struct-out message)
         (struct-out question-message))

(struct message (to resolve-me kws kw-vals args)
  #:transparent)

(struct question-message message (answer-this-question)
  #:transparent)
