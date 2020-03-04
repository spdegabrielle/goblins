#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          "eval.rkt")

@reset-eval![]

@title[#:tag "api"]{Goblins API}

@defmodule[goblins]

@section[#:tag "actors"]{Actors}

@define-footnote[actors-note make-actors-note]

@subsection[#:tag "what-is-an-actor"]{What is an "actor" in Goblins?}

Goblins implements the
@link["https://en.wikipedia.org/wiki/Actor_model"]{actor model}
on top of Racket / scheme.@actors-note{
  Sussman and Steele famously came to the conclusion that there was in
  fact no difference between actor-style message passing and procedure
  application in the lambda calculus, and indeed both are similar.
  However, there is a significant difference between synchronous
  call-and-return procedure application (which is what scheme implements
  in its most general form, and between actors in Goblins is handled
  by @racket[$]) and asynchronous message passing (which in Goblins
  is handled with @racket[<-]).}

The
@link["https://en.wikipedia.org/wiki/Actor_model#Fundamental_concepts"]{fundamental operations of actors}@actors-note{
Emphasis on "fundamental" operations.
Extensions happen from here and vary widely between different systems
that call themselves "actors".}
are:

@itemize[
  @item{An actor can send messages to other actors of which it has the
        address.
        (In Goblins, @racket[<-], possibly arguably @racket[$] as well.)}
  @item{An actor may create new actors.
        (In Goblins, @racket[spawn].)}
  @item{An actor may designate its behavior for the next message it
        handles.
        (In Goblins, this means returning a new message handler by
        wrapping that message handler in the actor's @tech{bcom}
        capability.)}]

Goblins' extensions to these ideas are:

@itemize[
  @item{@emph{Both} synchronous and asynchronous calls are defined and
        supported, between @racket[$] and @racket[<-] respectively.
        However, @racket[$] is both convenient and important for systems
        that much be transactionally atomic (eg
        @link["http://erights.org/elib/capability/ode/ode-capabilities.html"]{implementing money}),
        it is limited to objects that are within the same @tech{vat}.
        @racket[<-] is more universal in that any actor on any @tech{vat}
        may communicate with each other, but is asynchronous and cannot
        immediately return a resolved value.@actors-note{
          For more on why this is, see chapters 13-15 of
          @link["http://www.erights.org/talks/thesis/"]{Mark Miller's dissertation}..
          This was very influential on Goblins' design, including the decision
          to move from a coroutine-centric approach to an
          @link["http://www.erights.org/"]{E}-style promise approach.
          It could be that coroutines are re-added, but would have to be done
          with extreme care; section 18.2 of that same thesis for an
          explaination of the challenges and a possible solution for
          introducing coroutines.}}
  @item{Raw message passing without a clear way to get values back can
        be painful.
        For this reason, @racket[<-] implicitly returns a promise that
        can be resolved with @racket[on] (and, for extra convenience and
        fewer round trips, supports
        @secref["promise pipelining (tutorial)"]).@actors-note{
          In this sense, @racket[<-np], which does not return a promise,
          is closer to the foundational actors message passing.
          The kind of value that promises give us can be constructed manually
          by providing return addresses to @racket[<-np], but this is painful
          given how common needing to operate on the result of an operation is.}}
  @item{Goblins composes with all the existing machinery of Racket/scheme,
        including normal procedure calls.
        Instead, Goblins builds its abstractions on top of it, but none of this
        needs to be thrown away.@actors-note{
          Scheme is a beautiful language to build Goblins on top of, but
          actually we could build a Goblins-like abstraction layer on top
          of any programming language with sane lexical scoping and weak
          hash tables (so that actors which are no longer referenced can be
          garbage collected).}}]


@subsection{Constructors and bcom}

A @deftech{constructor} is a procedure which builds the first message
handler an actor will use to process messages / invocations.
The constructor has one mandatory argument, traditionally called
@tech{bcom} (pronounced "become" or "bee-com") which can be used
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

@deftech{bcom}, as shown above, is a capability (or technically a
"sealer") to become another object.
However, @tech{bcom} does not apply a side effect; instead,
it wraps the procedure and must be returned from the actor handler
to set that to be its new message handler.
Since this clobbers the space we would normally use to return a value
(for whatever is waiting on the other end of a @racket[$] or a @tech{promise}),
@tech{bcom} supports an optional second argument, which is that return value.
If not provided, this defaults to @racket[(void)].


@make-actors-note[]

@section[#:tag "core-procedures"]{Core procedures}

@define-footnote[core-api-note make-core-api-note]

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
Pronounced "call", "dollar call", or "money-call".@core-api-note{
Why "money call"?
Because you need @racket[$] to make
@link["http://erights.org/elib/capability/ode/index.html"]{
distributed ocap financial instruments}!}

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
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{continuation barrier}
is installed and so capturing of the continuation is not possible.
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

If @racketidfont{#:promise?} is set to @racket[#t], then @racket[on]
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

@make-core-api-note[]

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
Returns @racket[#t] if @racket[obj] is a @tech{live} reference.}

@defproc[(sturdy-refr? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is a @tech{sturdy} reference.}

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

@defproc[(near-refr? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is a @tech{near} @tech{reference}.}


@subsection{Local vs remote references}

Well, once @tech{machine}s exist, this will matter, but they don't yet :P


@section{Actormaps}

@define-footnote[actormaps-note make-actormaps-note]

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

In general, there are really two key operations for operating on
actormaps.
The first is @racket[actormap-spawn], which is really just used to
bootstrap an actormap with some interesting actors.
Actors then operate on @deftech[#:key "turn"]{turns}, which are
basically a top-level invocation; the core operation for that is
@racket[actormap-turn].
This can be thought of as like a toplevel invocation of a procedure at
a REPL: the procedure called may create other objects, instantiate and
call other procedures, etc, but (unless some portion of computation
goes into an infinite loop) will eventually return to the REPL with
some value.
Actormap turns are similar; actors may do
@seclink["core-procedures"]{anything that actors can normally do}
within the turn, including spawning new actors and calling other actors,
but the turn should ideally end in some result (as well as some new
messages to possibly dispatch).@actormaps-note{
  Due to the
  @link["https://en.wikipedia.org/wiki/Halting_problem"]{halting problem},
  this cannot be pre-guaranteed in a turing-complete environment such as
  what Goblins runs in.
  Actors can indeed go into an infinite loop; in general the security model
  of Goblins is to assume that actors in the same @tech{vat} can
  thus "hose their vat" (but really this means, an actormap turn might not
  end on its own, and vats currently don't try to stop it).
  Pre-emption can be layered manually though when operating on the actormap
  directly; if you want to do this, see
  @secref[#:doc '(lib "scribblings/reference/reference.scrbl")
          "threadkill"].}


@subsection{Actormap methods}

@defproc[(actormap? [obj any/c]) bool?]{
Determines if @racket[obj] is an @tech{actormap}.}

@defproc[(actormap-spawn [actormap actormap?] [constructor procedure?]
                         [arg any/c] ...)
         (values [actor-refr live-refr?] [new-actormap transactormap?])]{
Like @racket[spawn], but low-level and transactional.
Returns two values to its continuation, the new actor @tech{live}
@tech{reference}, and a @tech{transactormap} representing the change.}

@defproc[(actormap-spawn! [actormap actormap?] [constructor procedure?]
                          [arg any/c] ...)
         [actor-refr live-refr?]]{
Like @racket[actormap-spawn!], but directly commits the actor to
@racket[actormap].
Only returns the tech{reference} of the new actor.
No changes are committed in an exceptional condition.}

@defproc[(actormap-turn [actormap actormap?] [to-refr live-refr?] [args any/c] ...)
         (values [result any/c] [new-actormap transactormap?]
                 [to-near (listof message?)] [to-far (listof message?)])]{
Similar to performing @racket[$], applying @racket[args] to
@racket[to-refr], but transactional and a little bit cumbersome to use.
(In many cases, you'll prefer to use @racket[actormap-peek],
@racket[actormap-poke!], @racket[actormap-run], or @racket[actormap-run!]
which are easier.)
Returns four values to its continuation: the result of applying
@racket[args] to @racket[to-refr], a transactional new actormap, and
two lists of messages that may need to be sent (one to near actors, one
to far actors).}


@defproc[(actormap-reckless-poke! [actormap whactormap?] [to-refr live-refr?]
                                  [arg any/c] ...)
         [actor-refr live-refr?]]{
Like @racket[actormap-poke!], but only usable on a @tech{whactormap}
and mutates all bcom-effects immediately to the mapping.
A little bit faster but non-transactional... corrupt state can occur
in the case of exceptional conditions, as the system will not "roll
back".
@emph{Use with caution!}}

@defproc[(actormap-poke! [actormap actormap?] [to-refr live-refr?] [args any/c] ...)
         any/c]{
Similar to performing @racket[$], applying @racket[args] to
@racket[to-refr].
Commits its result immediately, barring an exceptional condition.}

@defproc[(actormap-peek [actormap actormap?] [to-refr live-refr?] [args any/c] ...)
         void?]{
Like @racket[actormap-poke!], but does not commit its result.
Useful for interrogating an actor in an actormap without allowing for
become-effects within it.}

@defproc[(actormap-run [actormap actormap?] [proc (-> any/c)]) any/c]{
Run @racket[proc], which is a thunk (procedure with no arguments) in the
actormap context, but do not commit its results, instead returning its
value.

Like @racket[actormap-peek], this is useful for interrogating an
actormap, but can be useful for doing several things at once.}

@defproc[(actormap-run! [actormap actormap?] [proc (-> any/c)]) any/c]{
Like @racket[actormap-run] but, barring exceptional conditions, does
commit its results.}


@subsection{whactormap}

A @deftech{whactormap} is the default kind of @tech{actormap}; uses a weak
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

A @deftech{transactormap} is an @tech{actormap} that stores a delta of
its changes and points at a previous actormap.
It must be committed using @racket[transactormap-commit!] before its changes
officially make it into its parent.

@defproc[(transactormap? [obj any/c]) bool?]{
Returns @racket[#t] if @racket[obj] is a @tech{transactormap}.}

@defproc[(make-transactormap [parent actormap?]
                             [#:vat-connector vat-connector
                              (or/c procedure? #f)
                              #f])
         transactormap?]{
Makes a new transactormap which is not yet committed and does not have
any new changes.
It is unlikely you will need this procedure, since @racket[actormap-turn],
@racket[actormap-spawn] and friends produce it for you.}

@defproc[(transactormap-merge! [transactormap transactormap?]) void/c]{
Recursively merges this and any parent @tech{transactormap}s until it reaches
the root @tech{whactormap}.

Note that creating two forking timelines of transactormaps upon a
whactormap and merging them may corrupt your whactormap.}

@defproc[(transactormap-merged? [transactormap transactormap?]) bool?]{
Returns @racket[#t] if this @tech{transactormap} has been merged.}

@defproc[(transactormap-parent [transactormap transactormap?]) actormap?]{
Returns the parent actormap of this transactormap.}

@defproc[(transactormap-delta [transactormap transactormap?]) hasheq?]{
Returns the delta of changes to this transactormap.
Mutating this yourself is not prevented but is highly inadvisable.}


@subsection{Snapshotting and restoring actormaps}

@defproc[(snapshot-whactormap [whactormap whactormap?]) hasheq?]{
Snapshots a whactormap by transforming it into a @racket[hasheq?]
table mapping @tech{reference}s to
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{ephemeron}
wrapped actor handlers.}

@defproc[(hasheq->whactormap [ht hasheq?]) whactormap?]{
Restores a @tech{whactormap} from the @racket[ht] snapshot.}


@subsection{Extra actormap procedures}

@defmodule[(submod goblins/core actormap-extra)]

These are very low level but can be useful for interrogating an
actormap.

@bold{TODO:} document.


@subsection{Vat connectors}

Somewhat awkwardly named since they most visibly show up in actormaps,
a @deftech{vat connector} is a procedure (or @racket[#f]) which is
attached to an actormap.  It serves two purposes:

@itemize[
  @item{To tell whether or not two actor references are @tech{near}
        each other.
        If both share the same @tech{vat connector}, then they are
        considered @tech{near}.@actormaps-note{
          There is one case in which this could be misleading: if both
          references are spawned in different actormaps that have no
          vat connector (ie, it is @racket[#f]), then they likely
          won't appear in each others' vats.}}
  @item{In case actors are not near each other, this specifies how to
        reach the actor.
        The procedure is called to communicate with the remote actormap,
        probably by dropping a message into the queue of its vat event
        loop.}]

If you are using @racket[make-actormap], this defaults to @racket[#f],
meaning that all other actors that also have no vat connector will
assume they are likewise near.
This of course also means that an actor which @emph{is} in a vat will
have no way of communicate with an actor which isn't.

On the other hand, @tech[#:key "vat"]{vats} built with
@racket[make-vat] set up their own vat connectors for you.

@make-actormaps-note[]

@section{Vats}

@define-footnote[vat-note make-vat-note]

A @deftech{vat}@vat-note{
"Vat" might strike you as a strange name; if so, you're not alone.
The term apparently refers to the musing, "How do you know if you're a
brain in a vat?"
Previously "vat" was called "hive" in Goblins (and its predecessors,
@link["https://www.gnu.org/software/8sync/"]{8sync} and
@link["https://xudd.readthedocs.io/en/latest/"]{XUDD});
it was an independent discovery of the same concept in
@link["http://www.erights.org/"]{E}.
Initially, Goblins stuck with "hive" because the primary author of
Goblins thought it was a more descriptive term; various ocap people
implored the author to not further fragment ocap vocabulary and so
the term was switched.
Since then, a number of readers of this documentation have complained
that "vat" is confusing, and upon hearing this story have asked for
the term to be switched back.
Whether it's better to avoid naming fragmentation or possibly increase
naming clarity is currently up for debate; your feedback welcome!}
is an event loop that wraps an @tech{actormap}.
In most cases, users will use vats rather than the more low-level
actormaps.

Actors in vats can communicate with actors in other vats on the same
machine over a @tech{vat connector}.
For inter-machine communication, see @tech{machines}.
Nonetheless, for the most part users don't need to worry about this
as most inter-vat communication happens using @racket[<-].

@; TODO: Document the #:private-key option?
@defproc[(make-vat) procedure?]{
Starts up a vat event loop.

Returns a procedure that can be invoked to communicate with the vat
(documented below).

The returned procedure uses symbol-based method dispatch.}

The procedure returned from @racket[make-vat] is called the
@deftech{vat dispatcher} and is mostly used for bootstrapping or
otherwise poking at a vat.
Once the actors are bootstrapped in a vat they tend to do their
own thing.

The @tech{vat dispatcher} supports the following symbol-dispatched
methods:

@itemize[
  @item{@racket['spawn]: Behaves like @racket[spawn] or @racket[actormap-spawn!].}
  @item{@racket['run]: Behaves like @racket[actormap-run!]}
  @item{@racket['call]: Behaves like @racket[$] or @racket[actormap-poke!]}
  @item{@racket['is-running?]: Is this vat still running?}
  @item{@racket['halt]: Stops the next turn from happening in the vat loop.
        Does not terminate the current turn, but maybe it should.}]

@make-vat-note[]

@section{Promises}

@define-footnote[promise-note make-promise-note]

An eventual send with @racket[<-] returns a @deftech{promise}
whose resolution will happen at some future time, either being
@deftech{fulfilled} or @deftech{broken}.
Fulfilled promises have resolved to a value, whereas broken promises
have been resolved with an error.

Every @tech{promise} has a corresponding @deftech{resolver} which is
messaged (often implicitly on completion of a turn)

It turns out that it is possible to make your own promises using
@racket[spawn-promise-values] or @racket[spawn-promise-cons],
both of which return a @tech{promise} / @tech{resolver} pair.

@defproc[(spawn-promise-values)
         (values [promise live-refr?] [resolver live-refr?])]{
Spawns and returns two values to its continuation: a
@racket{promise} and a @racket{resolver}.}

@defproc[(spawn-promise-cons)
         (cons/c [promise live-refr?] [resolver live-refr?])]{
Just like @racket[spawn-promise-values] but returns a cons cell of the
@racket{promise}/@racket{resolver} pair rather than returning multiple
values.
This requires an extra allocation (and thus destructuring on the
receiving side) but can be convenient since actors can only return
one value from their message handler at a time.}

Promises in Goblins work closer to @link["http://erights.org/"]{E}
than some other languages like
@link["https://en.wikipedia.org/wiki/JavaScript"]{Javascript};
notable exceptions are that @racket[on] is used rather than
".then() sausages".
Likewise, having a separate resolver object also comes from E.

One major, but perhaps not very important, difference that may not be
obvious is that once a promise pointed to by a @tech{reference} is
resolved to something, it for all intents and purposes appears to act
just like that thing.
If the resolution is to a @tech{near} reference, it can even be
immediately called with @racket{$}.
However, you still need to know when you can finally dollar-call such
a thing, thus you still need to use @racket[on] anyway.

@section{Machines}

A @deftech{machine} is another layer of abstraction you don't need to
worry about... yet! ;)
