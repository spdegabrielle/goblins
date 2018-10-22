#lang scribble/manual

@(require racket/sandbox
          scriblib/footnote
          scribble/example
          (prefix-in scribble-eval: scribble/eval)
          scribble/manual)

@(define my-evaluator
   (make-evaluator 'racket))

@(define-syntax-rule (interact e ...)
  (examples #:eval my-evaluator
            #:label #f
            e ...))

@(define-syntax-rule (interact-errors e ...)
  (scribble-eval:interaction #:eval my-evaluator
                             e ...))

@title{Goblins: a lightweight actor library}

Goblins is a lightweight actor model library for Racket.
It doesn't require a special @racketidfont{#lang}, though it may be
mixed specialized @racketidfont{#lang}s.

It is inspired by such object capability actor languages as the
@hyperlink["http://erights.org"]{E}, bringing strong security mixed
with support for highly distributed computing.
@note{More accurately, support for highly distributed computing coming
soon.}

@section{Goblins by example}


@section{High level concepts}




