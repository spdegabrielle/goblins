#lang scribble/manual

@(require racket/sandbox
          scriblib/footnote
          scribble/example
          (prefix-in scribble-eval: scribble/eval)
          scribble/manual)

@(define my-evaluator
   (make-base-eval #:lang 'racket))

@; @(define my-evaluator
@;    (parameterize ([sandbox-output 'string]
@;                   [sandbox-error-output 'string]
@;                   [sandbox-memory-limit 50])
@;      (make-evaluator 'racket)))

@; @(require racket/sandbox
@;           scriblib/footnote
@;           scribble/example
@;           (prefix-in scribble-eval: scribble/eval)
@;           scribble/manual
@;           teachpack/2htdp/scribblings/img-eval
@;           (for-label racket
@;                      pict pict/flash)
@;           (for-syntax racket/base))

@; @(define my-evaluator
@;    (make-img-eval))

@(define-syntax-rule (interact e ...)
  (examples #:eval my-evaluator
            #:label #f
            e ...))

@(define-syntax-rule (interact-errors e ...)
  (scribble-eval:interaction #:eval my-evaluator
                             e ...))

@title{Goblins: a lightweight actor library}

@emph{
  Goblins is currently pre-alpha.
  The semantics can and will change.
  Experimenting with the library is encouraged and welcome, but please
  don't use it for production-oriented code yet.
  Please especially be aware of
  @hyperlink["https://gitlab.com/spritely/goblins/issues/8"]{this known bug}.}

Goblins is a lightweight actor model library for Racket.
It doesn't require a special @racketidfont{#lang}, though it may be
mixed specialized @racketidfont{#lang}s.

It is inspired by such object capability actor languages as the
@hyperlink["http://erights.org"]{E}, bringing strong security mixed
with support for highly distributed computing.
(More accurately, support for highly distributed computing
@hyperlink["https://gitlab.com/spritely/goblins/issues/3"]{coming soon}.)

Goblins is designed as the foundational layer for the
@hyperlink["https://dustycloud.org/blog/spritely/"]{Spritely project}
but is designed to be fairly general so it can be used outside
of it as well.
This work is funded by people like you, so if you consider it valuable,
please consider
@hyperlink["https://www.patreon.com/cwebber"]{supporting this work}.

@section{Goblins by example}

@subsection{Our first actor interaction}

First we'll need to import Goblins, like so:

@interact[
  (require goblins)]

Spawning an actor in goblins should look fairly familiar.
Here's a simple "greeter" actor.

@interact[
  (define greeter
    (spawn
     (lambda (name)
       (format "Hello, ~a!" name))))]

Calling our greeter looks fairly similar to calling a function.
Here's the easy way to do it:

@interact[
  (<<- greeter "Samantha")]

@subsection{But why actors?}

In fact, that looks a little too similar to calling a function.
At this point you may be wondering, why the extra hassle of this
@racketidfont{<<-} thing?
Why not just define a function and call it normally?

There are really two answers to that question.
One answer is that actors are asynchronous and communicate via message
passing.
Each actor handles one incoming message at a time and in response
may change its own behavior/state, spawn other actors, or send messages
to other actors it holds references to.
In the CAP theorem of "Consistent, Available, Partition-tolerant, pick
two", Goblins chooses Available and Partition-tolerant.
(Eventual consistency may be modeled on a higher layer of abstraction,
but is not provided by Goblins itself.)

Another answer is that our actors can, in theory, live anywhere... on
another server, on the same machine in another process, etc etc.
For example, our greeter is actually an address to an actor
somewhere:

@interact[greeter]

@note{In the future you'll have to call an explicit method to see
the printed address of an actor reference.}
As we can see, this is a local actor reference.
Actors live in a "hive", usually with some other actors, and take
"turns" handling messages one at a time, kind of like turns in a board
game.
However, there may be other hives out there, and our actors can
still interact with them.
Actors in the same hive are both local, whereas actors in differing
hives have remote references.
This means that programmers can implement code with actors
interacting with each other without much care for where the actors
live.
@note{Remote actor interactions coming in probably Goblins v0.2}
In the abstract, an actor's address looks like
@racketidfont["<actor-id>@<hive-id>"], with @racketidfont["<actor-id>"]
being a large, unguessable random bytestring and @racketidfont["<hive-id>"]
being the fingerprint of a public key.
(However, both ids are calculated lazily, as necessary.
Local references can get away with in-process object references
without needing the overhead of generating ids.)

@subsection{More actor interactions}

That's enough theory; let's see some more code.

@subsubsection{Actors talk to actors}


Of course, actors can (and should) send each other messages:

@interact[
  (define alice
    (spawn
     (lambda ()
       'hello-i-am-alice)))
  (define bob
    (spawn
     (lambda (friend)
       (format "My friend says: ~a"
               (<<- friend)))))
  (<<- bob alice)]

@subsubsection{Object'y actors}

Not just procedures can be actors... anything that defines the
actor generic interface can be actors.
Along with procedures, class-based objects are one such type in
Racket which qualifies as an actor.
Observe:

@interact[
 (define animal%
   (class object%
     (super-new)
     (init-field name noise
                 [hunger 50])
     (define/public (hungry?)
       (> hunger 0))
     (define/public (chat)
       (format "<~a> ~a!! I'm feeling ~a!"
               name noise
               (if (hungry?)
                   "hungry"
                   "stuffed")))
     (define/public (feed kibbles)
       (set! hunger (- hunger kibbles)))))
 (define cat
   (spawn-new animal%
              [name "cat"]
              [noise "Meow"]))
 (<<- cat 'chat)]
    
As we can see, we can call public methods on our class-based actors,
but we need to specify the method by its symbol.

@subsubsection{<- (eventual send) and <<- (splitchronous send)}

Anyway, our poor cat is hungry!
We should probably feed it.

@interact[
 (<- cat 'feed 30)]

Hm, so this is interesting!
Instead of @racketidfont{<<-} we used @racketidfont{<-}.
@note{Early lisps experimenting with message passing used
@racketidfont{<-} for @racketidfont{send}; Goblins carries on
that tradition, and avoids a conflict with Racket's @racket{send}
expression in the process.}
We pronounce @racketidfont{<-} "eventual send" and
@racketidfont{<<-} "splitchronous send" (punned, just as you'd
guess, on "synchronous send"... except it's not synchronous, since
our actor remains available to handling other messages).
What's the difference?

Well, you may have noticed that using @racketidfont{<<-} looks very
much like running everyday straight-ahead synchronous code.
We send off our message, and a "response" comes back in, returning
whatever values (or raising whatever exception) is appropriate.
Inside of an actor's turn, calling @racketidfont{<<-} "splits" the
turn; the actor's currently executing code suspends (via delimited
continuation magic) until a "response" comes back from the actor
we sent the message to, at which case the code which was waiting
is resumed on another turn.
However, we remember that we have chosen to be highly "available",
so our actor is not blocked while waiting for a message to come
back; it can handle other messages in the interim.
(Outside the actor turn environment, such as at the REPL as we've been
playing in here, calling @racketidfont{<<-} @emph{does} block,
since we don't have a way to suspend-and-be-woken-up-later,
even though we may still wish to write straight-ahead code that
interfaces with actors.)
The splitting detail is very important!
We will see why later.

@racketidfont{<-} is different; instead of suspending in wait of some
response to come back, it shoots off the message and continues on its
way, no splitting the turn or anything.
Eventually it should probably get there.

However... we do see it returned something.
This thing is a promise, and we can listen to it if we want to.
We won't just yet.

Is our cat still hungry though?

@interact[
  (<<- cat 'chat)]

Of course it is; our cat takes 50 kibbles to be full, so we'll
need to feed it again.
Another 30 will put it over the edge and then some.

However, we don't actually need that promise that @racketidfont{<-}
returned in this case; we just want to feed the cat and get back
to whatever we were doing.
The default for @racketidfont{<-} is always to return a promise,
but this can be wasteful.
@note{It might be that this is the wrong default, and that by
default @racketidfont{<-} shouldn't return a promise, and instead
a promise needs to be requested specifically.
This part of the API may change sometime in the near future.}
If we don't care about the promise, as an optimization we can
use @racketidfont{<-np} (for "<- no promise") instead, which
doesn't produce a promise.

At the moment, we don't care about listening to it, so let's use
@racketidfont{<-np}.

@interact[
  (<-np cat 'feed 30)]

Up until now we've used @racketidfont{<<-} to chat with our cat.
This was convenient because the value was "returned" to us.
But remember that @racketidfont{<-} returned a promise?
We can use @racketidfont{on} to listen to it.
Here's some code we could run:

@codeblock|{
  (on (<- cat 'chat)
      (lambda (val)
        (displayln (format "Got back: ~a" val)))
      #:catch
      (lambda (err)
        (displayln "Uhoh, something is wrong with our cat!")))
  ;; Should print out "Got back: <cat> Meow!! I'm feeling stuffed!"
}|

The first argument to @racketidfont{on} is the promise we'd like
to listen to.
The second argument is a handler for when the promise succeeds.
The procedure supplied to the @racketidfont{#:catch} keyword
allows us to do error handling.

Let's observe that while @racketidfont{<<-} returns the
value to its continuation, handling a promise from @racketidfont{<-}
with @racketidfont{on} does not.
So instead of returning the value, we printed it out to the screen
in this example.
@note{Though in a real actor system we generally want to
have one actor handle responsibility for a resource, so maybe we'd
set up an actor which handles displaying text to the screen.}

@subsubsection{Splitchronous anywhere execution vs synchronous local execution}

It's important to re-emphasize though that @racketidfont{<<-}
"splits" up the turn.
This is important to realize, in case you wrote an actor like
the following:

@codeblock|{
  ;; An actor that supposedly feeds animals 10 kibbles
  ;; but only if they're hungry
  (define feeds-when-hungry
    (spawn
      (lambda (animal)
        (cond [(<<- animal 'hungry?)
               ;; race condition between above and below lines!
               (<-np animal 'feed 10)
               'fed-it]
              [else 'didnt-feed-it]))))
}|

See the bug?
We check if our animal is fed, but since we're using a "splitchronous"
operation, this turn is split into two... between when the animal tells
us if it's hungry or not and our next turn where we choose to feed
it or not, someone else might have swept in and fed the animal as well.
Thus, we might accidentally overfeed our pet!

But... consider that this is still Racket, and we still have normal
Racket sychronous code available to us.
Local actors can provide an interface which gives support for direct,
synchronous, immediate calls.
This interface will not survive transfer between remote vats, so is
safe to hand out with the assurance that it is for local-only use.
This is safe from corruption because our actors only operate one turn
at a time.
@note{But wait, doesn't that mean an actor that goes into a busy
loop can hose all the rest of the actors in the same Hive?
The answer is "yes", but it's possible to add on per-actor pre-emption.
@emph{@bold{TODO:} Explain how!}}

@interact[
  (define animal2%
    (class animal%
      (super-new)
      (define/public (direct-access)
        this)))
  (define crow
    (spawn
     (new animal2%
          [name "crow"]
          [noise "Caw, caw"]
          [hunger 20])))]

Now we can define and use a safer feeds-when-hungry kind of actor:

@interact[
  (define feeds-when-hungry2
    (spawn
      (lambda (animal)
        (define direct-animal
          (<<- animal 'direct-access))
        (cond [(send direct-animal hungry?)
               ;; race condition between above and below lines!
               (send direct-animal feed 10)
               'fed-it]
              [else 'didnt-feed-it]))))
  (<<- feeds-when-hungry2 crow)
  (<<- feeds-when-hungry2 crow)
  (<<- feeds-when-hungry2 crow)]

And we can be assured that the crow really wasn't overfed, because
we made use of the synchronous local interface.

@subsubsection{Promises upon promises}

If you've used some other systems that have promises or callbacks,
you may be familiar with the "pyramid of doom": your promise handlers
keep nesting inward and inward and inward.
Here's a contrived example:

@interact[
 (define car%
   (class object%
     (super-new)
     (init-field make model color)
     (define/public (description)
       (format "~a ~a ~a" color make model))
     (define/public (drive mph)
       (define movement
         (cond
           [(< mph 0) "moving backwards"]
           [(= mph 0) "sitting still"]
           [(< mph 15) "crawling along the road"]
           [(< mph 30) "driving leisurely"]
           [(< mph 60) "driving along"]
           [(< mph 80) "speeding along"]
           [else "blasting down the highway"]))
       (format "The ~a is ~a!"
               (description) movement))))
 (define (spawn-car-factory default-make default-model default-color)
   (spawn
    (lambda (#:make [make default-make]
             #:model [model default-model]
             #:color [color default-color])
      (spawn-new car%
                 [make make]
                 [model model]
                 [color color]))))
 (define honda-factory (spawn-car-factory "honda" "civic" "black"))]

Now we can create cars with the @racketidfont{honda-factory} and drive
them.
If we wanted to make a car and print out the text from a driving
interaction in one swoop, the most familiar code style for doing this
can be achieved with the @racketidfont{<<-} operator:

@interact[
  (displayln (<<- (<<- honda-factory #:color "blue")
                  'drive 16))]

If we wanted to use @racketidfont{<-}, we could do so like this:

@codeblock|{
 (on (<- honda-factory #:color "blue")
     (lambda (a-car)
       (on (<- a-car 'drive 16)
           (lambda (drive-desc)
             (displayln drive-desc)))))
}|

But this is kind of hard to read, despite only being two message sends
deep.
This might discourage you from using the @racketidfont{<-} operator,
but it has some extra power that the @racketidfont{<<-} does not:
we can do eventual sends to promises and get back more promises.
@note{This cool design taken from the
@hyperlink["http://erights.org/"]{E programming language}, laboratory
of many great ideas.}
That probably sonds confusing, so see for yourself:

@codeblock|{
 (on (<- (<- honda-factory #:color "blue") 'drive 16)
     (lambda (drive-desc)
       (displayln drive-desc)))
}|

See?

So what advantage does this have over using @racketidfont{<<-}?
Two things: the rest of our code can proceed forward immediately;
this may be useful if we have many messages to send out.
We also save a round trip of "waking up" this suspended handler
again.
Best of all, once distributed inter-hive messaging lands in Goblins,
we will be able to take advantage of the efficient, round-trip-avoiding
power of
@hyperlink["http://erights.org/elib/distrib/pipeline.html"]{promise pipelining}.

@subsubsection{When to use what?}

So when to use @racketidfont{<-} vs @racketidfont{<<-}
vs plain ol' synchronous Racket code?

To be written :)

@; @itemize[
@;   @item{Use @racketidfont{<-} (eventual send) when:
@;     @itemize{
@;       @item{}
@;     }}
@;   @item{use @racketidfont{<<-} (splitchronous send) when:
@;     @itemize{
@;       @item{}
@;     }}]

@section{How does <<- work?}

It's not critical to understand as a user, so if this section
breaks your brain, ignore it.

Technically @racketidfont{<<-} is built on top of @racketidfont{<-}.
(Well technically on top of @racketidfont{send-message}, which
permits an additional argument of "resolve this promise
once the handler of this message completes".)
There's a delimited continuation "prompt" right above the actor event
loop, and what @racketidfont{<<-} does is it "suspends" the current
code to that prompt, calls the target using @racketidfont{<-} passing
in all arguments, and then listens to the resulting promise with
@racketidfont{on}, setting up the listener to resume the continuation
of the handler with the response values on success, and on error
to propagate the exception.

So, it's not magic... no more so than delimited continuations normally
are. :)
