#lang racket

(provide start-client)

(require racket/unix-socket
         goblins/captp
         pk
         syrup)

(define things-to-say
  '((howdy partner)
    (how go the "cowpokes" up on "farm" 33.2 "?")
    (its mighty #"hot" down here)))

(define (start-client unix-domain-socket-path)
  (define-values (ip op)
    (unix-socket-connect unix-domain-socket-path))
  (pk 'client-connected 'ip ip 'op op)
  (for ([sayin things-to-say])
    (syrup-write sayin op))
  (flush-output op)
  (values ip op))

