#lang scribble/manual

@(require racket/sandbox
          scriblib/footnote
          scribble/example
          (prefix-in scribble-eval: scribble/eval)
          scribble/manual)

@(define my-evaluator
   (make-base-eval #:lang 'racket))

@(define-syntax-rule (interact e ...)
  (examples #:eval my-evaluator
            #:label #f
            e ...))

@(define-syntax-rule (interact-errors e ...)
  (scribble-eval:interaction #:eval my-evaluator
                             e ...))

@title{Goblins: a lightweight actor library}

Sorry!  There's been a massive rewrite and all these docs need a
rewrite. :)
