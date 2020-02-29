#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          "eval.rkt")

@title{API}

@defmodule[goblins]

@section{Core procedures}

@; spawn

@; call $  ; $ is an alias
@; <- <-np

@; on
@; extract

@; refr?
@; live-refr?
@; sturdy-refr?
@; near-refr?

@section{Vats}

@; TODO: Document the #:private-key option?
@defproc[(make-vat) procedure?]{
Starts up a vat event loop.
Returns a procedure that can be invoked to communicate with the vat.

The returned procedure uses symbol-based method dispatch.

@bold{TODO:} document all the methods.}


@section{Actormaps}

