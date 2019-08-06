#lang racket/base

(provide (struct-out message))

(struct message (to resolve-me kws kw-vals args))
