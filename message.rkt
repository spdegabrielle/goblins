#lang racket-base

(provide (struct-out message))

(struct message (to kws kw-vals args))
