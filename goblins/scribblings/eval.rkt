#lang racket

(require racket/sandbox
         scribble/manual
         scribble/example
         (prefix-in scribble-eval: scribble/eval))

(provide goblins-evaluator
         interact
         interact-errors
         run-codeblock
         hidden-eval)

(define goblins-evaluator
  (make-base-eval #:lang 'racket))

(define-syntax-rule (interact e ...)
  (examples #:eval goblins-evaluator
            #:label #f
            e ...))

(define-syntax-rule (interact-errors e ...)
  (scribble-eval:interaction #:eval goblins-evaluator
                             e ...))

(define-syntax-rule (run-codeblock e ...)
  (begin
    (goblins-evaluator
     (call-with-input-string (string-join (list e ...) " ")
       read))
    (codeblock e ...)))

(define-syntax-rule (hidden-eval e ...)
  (examples #:eval goblins-evaluator
            #:hidden #t
            e ...))
