#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          "eval.rkt")

@reset-eval![]

@title{API}

@defmodule[goblins]

@section{Actor constructors}

A @deftech{constructor} is a procedure which builds the first message
handler an actor will use to process messages / invocations.
The constructor has one mandatory argument, traditionally called
@racket[bcom] (pronounced "become" or "bee-com") which can be used
to set up a new message handler for future invocations.

@interact[
(require goblins)
(define am (make-actormap))

(code:comment "Outer procedure is the constructor.")
(code:comment "Implicitly takes a bcom argument.")
(code:comment "count argument may or may not be supplied by spawner.")
(define (^noisy-incrementer bcom [count 0])
  (code:comment "Our message handler.")
  (lambda ([increment-by 1])
    (let ([new-count (+ count increment-by)])
      (code:comment "Here we create a new version of ^noisy-incrementer")
      (code:comment "with count scoped to a new incremented version.")
      (code:comment "The second argument to bcom specifies a return value,")
      (code:comment "would return void if unspecified.")
      (bcom (^noisy-incrementer bcom new-count)
            (format "My new count is: ~a" new-count)))))
(define incr1
  (actormap-spawn! am ^noisy-incrementer))
(actormap-poke! am incr1)
(actormap-poke! am incr1 20)
(define incr2
  (actormap-spawn! am ^noisy-incrementer 18))
(actormap-poke! am incr2 42)]

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

The @racket[constructor] is, as the name sounds, a goblins
@tech{constructor}, and is first passed a @racket[bcom] argument (which
is a way specify how it will behave on its next invocation) and then
is passed the remaining @racket[argument]s.}

@; call $  ; $ is an alias

@defproc[($ [actor-refr near-refr?] [arg any/c] ...) any/c]{
Provide a synchronous call against the current message handler of
@racket[actor-refr], which must be a @tech{near} @racket[live-refr?]
in the same vat as the currently running actor context.
The value returned is that which is returned by the
@racket[actor-refr]'s message handler upon invocation (or, if
@racket[actor-ref] chose to @racket[bcom] something else, the second
argument passed to its @racket[bcom], or void if not provided.)
Exceptions raised by @racket[actor-refr]'s invocation will be
propagated and can be captured.

Note that excape continuations can be set up between a caller of
@racket[$] and can be used by the callee.  However, a 
@tech{continuation barrier} is installed and so capturing of the
continuation is not possible.
(Note that in the future this may be relaxed so that async/await
coroutines can be used with mutual consent of caller/callee; whether
or not this is a good idea is up for debate.)}

@; <- <-np

@defproc[(<- [actor-refr live-refr?] [arg any/c] ...)
         [promise live-refr?]]{
Like @racket[$], but called asynchronously and returns a @tech{promise},
which may be handled with @racket[on].
Unlike @racket[$], @racket[<-] is not limited to only @tech{near}
@racket[actor-refr]s; interaction with @tech{far} actors are permitted
as well.}

@defproc[(<-np [actor-refr live-refr?] [arg any/c] ...)
         void?]{
Like @racket[<-] but does not return (or require the overhead of) a
promise.
@racket[<-np] is effectively an optimization; where promises are not
needed, using @racket[<-np] over @racket[<-] is a good idea.}

@; on
@defproc[(on [vow (or/c live-refr? any/c)]
             [on-fulfilled (or/c procedure? refr? #f) #f]
             [#:catch on-broken (or/c procedure? refr? #f) #f]
             [#:finally on-finally (or/c procedure? refr? #f) #f]
             [#:promise? promise? bool? #f])
         (or/c live-refr? void?)]{
Sets up promise handlers for @racket[vow], which is typically a
@racket[live-refr?] to a @tech{promise} but may be anything.

@racket[on-fulfilled], @racket[on-broken], and @racket[on-finally]
all, if specified, may be either a procedure (the most common case)
which is run when @racket[vow] becomes resolved, or a reference to an
actor that should be messaged when the promise is resolved with the
same arguments that would be passed to an equivalent procedure.
As their names suggest, @racket[on-fulfilled] will run if a promise is
fulfilled and takes one argument, the fulfilled value.
@racket[on-broken] will run if a promise is broken and takes one
argument, the error value.
@racket[on-finally] will run when a promise is resolved, no matter the
outcome and is called with no arguments.

If @racketidfont{#:promise?} is set to @racket{#t}, then @racket[on]
will itself return a promise.
The promise will be resolved as follows:

@itemize[
  @item{If @racket[vow] is fulfilled and @racket[on-fulfilled] is not
        provided, the promise returned will share @racket[vow]'s resolution.}
  @item{If @racket[vow] is broken and @racket[on-broken] is not
        provided, the promise returned will share @racket[vow]'s broken result.}
  @item{If @racket[vow] is fulfilled and @racket[on-fulfilled] is provided,
        the resolution will be whatever is returned from @racket[on-fulfilled],
        unless @racket[on-fulfilled] raises an error, in which case the promise
        will be broken with its error-value set to this exception.}
  @item{If @racket[vow] is broken and @racket[on-broken] is provided,
        the resolution will be whatever is returned from @racket[on-broken],
        unless @racket[on-broken] raises an error, in which case the promise
        will be broken with its error-value set to this exception (which
        may even be the original exception).}]}

@; TODO: Do we document this?  Do we even keep it around? :P
@; extract


@section{References}

A @deftech{reference} is a capability to communicate with an actor.
References are an indirection and abstractly correspond to an actor handler
in an @tech{actormap} somewhere, often in a @tech{vat}.

@; refr?
@defproc[(refr? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is a @tech{reference}}.

@subsection{Live vs Sturdy references}

The most common kind of reference is a @deftech{live} reference,
meaning that they correspond to some actor which we have an
established connection to.
However some references may be @deftech{sturdy} references, meaning
they are serialized in such a way that they probably refer to some
actor, but the connection to it is dormant in this reference itself.
A sturdy reference must be enlivened with @racket[enliven] before it
can be used.
(@bold{TODO:} or do we just want to reuse @racket[<-]?  Dunno.)

@bold{NOTE:} Sturdy references aren't implemented yet, and neither is
@racket[enliven].
Change that!

@defproc[(live-refr? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is @tech{live}.}

@defproc[(sturdy-refr? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is @tech{sturdy}.}

@bold{TODO:} Define and document @racket[enliven], maybe.

@; live-refr?
@; sturdy-refr?
@; near-refr?

@subsection{Near vs far references}

An actor is @deftech{near} if it is in the same vat as the actor being
called in an actor context.
An actor is @deftech{far} if it is not.
The significance here is that only @tech{near} actors may perform immediate
calls with @racket[$], whereas any actor may perform asynchronous message
sends with @racket[<-] and @racket[<-np].

@subsection{Local vs remote references}

Well, once @tech{machine}s exist, this will matter, but they don't yet :P


@section{Actormaps}

An @deftech{actormap} is the key abstraction that maps actor
references to their current method handlers.
There are actually two kinds of actormaps, @tech{whactormap}s and
@tech{transactormap}s.

@defproc[(make-actormap [#:vat-connector vat-connector
                         (or/c procedure? #f)
                         #f])
         whactormap?]{
Alias for @racket[make-whactormap], since this is the most common
actormap users make.}

Actormaps are also wrapped by @tech{vat}s.
More commonly, users will use vats than actormaps directly; however,
there are some powerful aspects to doing so, namely for
strictly-synchronous programs (such as games) or for snapshotting
actormaps for time-traveling purposese.

@subsection{Actormap methods}

@subsection{whactormap}

A @deftech{whactormap} is the default kind of actormap; uses a weak
hashtable for mapping.

@defproc[(make-whactormap [#:vat-connector vat-connector
                            (or/c procedure? #f)
                            #f])
         whactormap?]{
Makes a weak hashtable actormap.
Used to mutably track the current state of actors.}

@defproc[(whactormap? [obj any/c]) bool?]{
Determines if @racket[obj] is a @tech{whactormap}.}


@subsection{transactormap}

@subsection{Extra whactormap methods}

@defmodule[(submod goblins/core actormap-extra)]

These are very low level but can be useful for interrogating an
actormap.


@section{Vats}

A @deftech{vat} is a blah blah

@; TODO: Document the #:private-key option?
@defproc[(make-vat) procedure?]{
Starts up a vat event loop.
Returns a procedure that can be invoked to communicate with the vat.

The returned procedure uses symbol-based method dispatch.

@bold{TODO:} document all the methods.}


@section{Promises}

A @deftech{promise} ...


@section{Machines}

A @deftech{machine} is another layer of abstraction you don't need to
worry about... yet! ;)
