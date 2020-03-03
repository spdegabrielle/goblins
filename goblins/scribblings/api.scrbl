#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          "eval.rkt")

@title{API}

@defmodule[goblins]

@section{Core procedures}

The following procedures are the core API used to write Goblins code.
All of them must be run within an "actor context", which is to say
either from an actor running within a @tech{vat} or an @tech{actormap}
or within one of the procedures used to bootstrap the vat/actormap.

@defproc[(spawn [constructor procedure?]
                [argument any/c] ...) live-refr?]{
Spawn an actor built by @racket[constructor] with the rest
of the @racket[argument]s being passed to that constructor.
Returns a live reference to the newly spawned actor.

The @racket[constructor] is first passed a @racket[bcom] argument
(which is a way specify how it will behave on its next invocation)
and then is passed the remaining @racket[argument]s.}

@; call $  ; $ is an alias

@defproc[($ [actor-refr live-refr?] [arg any/c] ...) any/c]{
Call @racket[actor-refr], which must be a @racket[live-refr?] in the
same vat as the currently running actor context.}



@; <- <-np

@; on
@; extract

@section{References}

@; refr?
@; live-refr?
@; sturdy-refr?
@; near-refr?

@section{Vats}

A @deftech{vat} is a blah blah

@; TODO: Document the #:private-key option?
@defproc[(make-vat) procedure?]{
Starts up a vat event loop.
Returns a procedure that can be invoked to communicate with the vat.

The returned procedure uses symbol-based method dispatch.

@bold{TODO:} document all the methods.}


@section{Actormaps}

An @deftech{actormap} is a blah blah
