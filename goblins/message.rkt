#lang racket/base

(provide (struct-out message)
         (struct-out question-message)
         (struct-out listen-request))

(struct message (to resolve-me kws kw-vals args)
  #:transparent)

(struct question-message message (answer-this-question)
  #:transparent)

(struct listen-request (to listener wants-partial?)
  #:transparent)
