#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          (for-label "api.scrbl")
          "eval.rkt")

@(define id racketidfont)

@title{A tutorial}

@section{Vats, actors, spawning, and immediate calls}

Let's open a Racket REPL and import Goblins:

@codeblock|{
#lang racket
(require goblins)
}|

@; For examples going forward
@(hidden-eval
  (require goblins racket))

First we're going to need something to store our objects in.
We'll boot up an event loop, called a "vat" (TODO: explain why it's
called that), which can manage our objects.

@interact[
  (define a-vat
    (make-vat))]

Our vat is currently lonely... nobody lives in it!
Let's make a friend.
First we'll need a friend constructor:

@run-codeblock|{
;; The ^ is conventionally called a "hard hat" in Goblins; it means
;; this is an object constructor.
;; Every constructor takes a bcom argument, the rest of them are
;; passed in from the spawn invocation.
(define (^friend bcom my-name)
  ;; This is the initial handler procedure.
  (lambda (your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name)))}|

The outer procedure is the constructor; all it really does is return
another procedure which is the handler.

Let's make a friend and call her Alice.

@run-codeblock|{
(define alice
  (a-vat 'spawn ^friend "Alice"))}|

Here the arguments to the spawn method are @id{^friend}, which is the
constructor procedure we are using, and the argument "Alice", which
becomes bound to @id{my-name}.
(The @id{bcom} argument is implicitly provided by Goblins; we'll
ignore it for right now.)

If we look at Alice in the REPL, we'll see that what we're really
holding onto is a "live reference" to Alice.

@interact[
  alice]

Now we'd like to talk to Alice.
For now let's use the "call" method on our vat:

@interact[
  (a-vat 'call alice "Chris")]

If we look at our @id{^friend} procedure again, this should make a lot
of sense; the inner lambda is being evaluated with "Chris" being
passed in for @id{your-name}.
The returned value is just the result.

Normally in a Goblins program, we can just spawn and talk to a Goblins
object directly using the @racket[spawn] and @racket[$] operators directly.
However we're kind of "bootstrapping the world" here, so we need the
vat's help to do that.
However, we can see what it would look like to use them if we were in
a "Goblins context" by using the vat's @id{'run} method
which allows us to pass in an arbitrary thunk (aka "procedure with no
arguments"):

@interact[
  (a-vat 'run
         (lambda ()
           (define alyssa
             (spawn ^friend "Alyssa"))
           ($ alyssa "Ben")))]

Anyway, maybe we'd like to be greeted the same way we have been by our
friend sometimes, but other times we'd like to just find out what our
friend's name is.
It would be nice to have different "methods" we could call, and in
fact, Goblins comes with a convenient @id{methods} macro:

@run-codeblock|{
(require goblins/actor-lib/methods)

(define (^methods-friend bcom my-name)
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name)]
   ;; return what our name is
   [(name)
    my-name]))}|

Now let's spawn an alice2 that uses @id{^methods-friend}:

@run-codeblock|{
(define alice2
  (a-vat 'spawn ^methods-friend "Alice"))}|

Now we can call each method separately:

@interact[
(a-vat 'call alice2 'name)
(a-vat 'call alice2 'greet "Chris")]

(As a side note, if you're thinking that it would look nicer if this
looked like:)

@codeblock|{
(a-vat.call alice2.name)
(a-vat.call alice2.greet "Chris")}|

(... you're right and a @id{#lang} will provided in the future for
such aesthetic improvement which expands to the former syntax.)

What kind of magic is this methods thing?
Well actually it's barely any magic at all.
Methods just returns a procedure that dispatches on the first argument,
a symbol, to one of several procedures.
We can even use it outside of an object/actor constructor:

@interact[
  (define what-am-i
    (methods
     [(i-am)
      (list 'i 'am 'just 'a 'procedure)]
     [(that what)
      (list 'that 'calls 'other what)]))
  (what-am-i 'i-am)
  (what-am-i 'that 'procedures)]

We could have just as well written methods-friend like so:

@run-codeblock|{
  (define (^match-friend bcom my-name)
    (match-lambda*
     ;; Greet the user using their name
     [(list 'greet your-name)
      (format "Hello ~a, my name is ~a!" your-name my-name)]
     ;; return what our name is
     [(list 'name)
      my-name]))}|

But it's a lot nicer to use @id{methods}.
But the key thing to realize is that @id{methods} just itself returns
another procedure.

Maybe we'd like to keep track of how many times our friend has been
called.
It might be helpful to have some kind of helper object which can do
that.
What if we made a counter?

@run-codeblock|{
(define (^counter bcom [count 0])
  (methods
   ;; return the current count
   [(count)
    count]
   ;; Add one to the current counter
   [(add1)
    (bcom (^counter bcom (add1 count)))]))}|

Now let's spawn and poke at an instance of that counter a bit:

@interact[
(define a-counter
  (a-vat 'spawn ^counter))
(a-vat 'call a-counter 'count)
(a-vat 'call a-counter 'add1)
(a-vat 'call a-counter 'count)
(a-vat 'call a-counter 'add1)
(a-vat 'call a-counter 'add1)
(a-vat 'call a-counter 'count)]

Now note that our counter actually appears to change... how does this
happen?
Let's look at the body of that add1 method in detail:

@codeblock|{
  (bcom (^counter bcom (add1 count)))}|

@id{bcom} (pronounced "be-come" or "be-comm") is the capability to
@id{become} a new version of ourselves; more explicitly, to give a
procedure which will be called the @emph{next time} this object is
invoked.
@id{next} returns some methods wrapped up in a closure which knows
what the count is; we're incrementing that by one from whatever we
currently have.
(Depending on how experienced you are with functional programming is
how confusing this is likely to be.)

There are multiple equivalent ways we could build the "next" procedure
we are becoming for ourselves, and one is to build an actual @id{next}
builder and instantiate it once.
This technique is exactly equivalent to the above, and we will use this
kind of structure later, so it's worth seeing and realizing it's more
or less the same (except that we didn't expose the choice of an initial
count value):

@run-codeblock|{
  (define (^counter bcom)
    (define (next count)
      (methods
       ;; return the current count
       [(count)
        count]
       ;; Add one to the current counter
       [(add1)
        ;; Become the next version of ourselves,
        ;; with count incremented
        (bcom (next (add1 count)))]))
    ;; We'll start at 0.
    (next 0))}|

Now that we have this counter, we can rewrite our friend to spawn it
and use it:

@run-codeblock|{
(define (^counter-friend bcom my-name)
  (define greet-counter
    (spawn ^counter))
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    ($ greet-counter 'add1)
    (define greet-count
      ($ greet-counter 'count))
    (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
            your-name my-name greet-count)]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    ($ greet-counter 'count)]))}|

You'll observe that there was no need to go through the vat here, our
object was able to use @racket[spawn] and @racket[$] (which is
pronounced "call", "immediate call", or "money call") directly.
That's because our actor is operating within a goblins context, so
there's no reason to do so by bootstrapping through the vat (indeed,
trying to do so would cause an exception to be raised).

Now let's give it a try:

@interact[
(define alice3
  (a-vat 'spawn ^counter-friend "Alice"))
(a-vat 'call alice3 'greet "Chris")
(a-vat 'call alice3 'greet "Chris")
(a-vat 'call alice3 'greet-count)
(a-vat 'call alice3 'greet "Chris")
(a-vat 'call alice3 'greet-count)]

Perhaps we'd like to have our friend remember the last person she
was called by.
It would be nice if there were something along the lines of a mutable
variable we could change.
In fact there is, and it's called a cell.
When called with no arguments, a cell returns its current value.
When called with one argument, a cell replaces its current value with
the one we have provided:

@interact[
(require goblins/actor-lib/cell)
(define treasure-chest
  (a-vat 'spawn ^cell 'gold))
(a-vat 'call treasure-chest)
(a-vat 'call treasure-chest 'flaming-sword)
(a-vat 'call treasure-chest)]

A fun exercise is to try to write your own cell.
Here is one way:

@run-codeblock|{
;; Constructor for a cell.  Takes an optional initial value, defaults
;; to false.
(define (^our-cell bcom [val #f])
  (case-lambda
    ;; Called with no arguments; return the current value
    [() val]
    ;; Called with one argument, we become a version of ourselves
    ;; with this new value
    [(new-val)
     (bcom (^our-cell bcom new-val))]))}|

Of course, you could also have a cell that instead has @id{'get} and
@id{'set} methods.
This is left as an exercise for the reader.

Now that we have cells, we can use them:

@run-codeblock|{
(define (^memory-friend bcom my-name)
  (define greet-counter
    (spawn ^counter))
  (define recent-friend
    (spawn ^cell #f))
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    ($ greet-counter 'add1)
    (define greet-count
      ($ greet-counter 'count))
    ;; Who our friend was last time
    (define last-friend-name
      ($ recent-friend))
    ;; But now let's set the recent friend to be this name
    ($ recent-friend your-name)
    (if last-friend-name
        (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!" 
                your-name my-name greet-count last-friend-name)
        (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
                your-name my-name greet-count))]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    ($ greet-counter 'count)]))}|

Let's try interacting with this friend:

@interact[
(define alice4
  (a-vat 'spawn ^memory-friend "Alice"))
(a-vat 'call alice4 'greet "Chris")
(a-vat 'call alice4 'greet "Carl")
(a-vat 'call alice4 'greet "Carol")]

Whew... if we look at that code for the @id{greet} code, it sure looks
fairly imperative, though.
We pulled out the value from @id{recent-friend} before we changed it.

If we had accidentlaly put the definition for @id{last-friend-name}
after setting @id{recent-friend} to @id{your-name}, we might have
resulted in a classic imperative error and @id{last-friend-name} would
be set to the new one instead of the old one!

Well, it turns out that @id{bcom} can take a second argument which
provides a value it would like to return in addition to specifying the
new version of itself it would like to become.
This means that we could rewrite @id{^memory-friend} like so with no
behavioral differences:

@run-codeblock|{
(define (^memory-friend2 bcom my-name)
  (define (next greet-count last-friend-name)
    (methods
     ;; Greet the user using their name
     [(greet your-name)
      (define greeting
        (if last-friend-name
            (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!" 
                    your-name my-name greet-count last-friend-name)
            (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
                    your-name my-name greet-count)))
      (bcom (next (add1 greet-count)
                  your-name)
            greeting)]
     ;; return what our name is
     [(name)
      my-name]
     ;; check how many times we've greeted, without
     ;; incrementing it
     [(greet-count)
      greet-count]))
  (next 1 #f))}|

@interact[
(define alice5
  (a-vat 'spawn ^memory-friend2 "Alice"))
(a-vat 'call alice5 'greet "Chris")
(a-vat 'call alice5 'greet "Carl")
(a-vat 'call alice5 'greet "Carol")]

This certainly looks more functional, and we have some freedom of how
we'd like to implement it.
It also leads to the observation that the behavior of objects in
respect to updating themselves appears to be very functional (returning
a new version of ourselves and maybe a value), whereas calling other
objects appears to be very imperative.
So what is the point of cells and counters?  After all, if we're using
@id{#lang racket} we have access to the @racket[set!]  procedure, and
could have easily rewritten the @id{^counter} and @id{^cell} versions
like so:

@run-codeblock|{
(define (^imperative-friend bcom my-name)
  (define greet-count
    0)
  (define recent-friend
    #f)
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    (set! greet-count (add1 greet-count))
    ;; Who our friend was last time
    (define last-friend-name
      recent-friend)
    ;; But now let's set the recent friend to be this name
    (set! recent-friend your-name)
    (if last-friend-name
        (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!" 
                your-name my-name greet-count last-friend-name)
        (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
                your-name my-name greet-count))]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    greet-count]
   [(recent-friend)
    recent-friend]))}|

Usage is exactly the ssame:

@interact[
(define alice6
  (a-vat 'spawn ^imperative-friend "Alice"))
(a-vat 'call alice6 'greet "Chris")
(a-vat 'call alice6 'greet "Carl")
(a-vat 'call alice6 'greet "Carol")]

This code looks mostly the same too, and indeed maybe even a little
simpler with @racket[set!] (no mucking around with that @racket[$]
malarky).

Let's introduce a couple of bugs into both the cell-using and
the @racket[set!] using imperative versions of these friends:

@run-codeblock|{
(define (^buggy-memory-friend bcom my-name)
  (define greet-counter
    (spawn ^counter))
  (define recent-friend
    (spawn ^cell #f))
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    ($ greet-counter 'add1)
    (define greet-count
      ($ greet-counter 'count))
    ;; Who our friend was last time
    (define last-friend-name
      ($ recent-friend))
    ;; But now let's set the recent friend to be this name
    ($ recent-friend your-name)
    (error "AHH! Throwing an error after I changed things!")
    (if last-friend-name
        (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!" 
                your-name my-name greet-count last-friend-name)
        (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
                your-name my-name greet-count))]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    ($ greet-counter 'count)]))

(define (^buggy-imperative-friend bcom my-name)
  (define greet-count
    0)
  (define recent-friend
    #f)
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    (set! greet-count (add1 greet-count))
    ;; Who our friend was last time
    (define last-friend-name
      recent-friend)
    ;; But now let's set the recent friend to be this name
    (set! recent-friend your-name)
    (error "AHH! Throwing an error after I changed things!")
    (if last-friend-name
        (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!" 
                your-name my-name greet-count last-friend-name)
        (format "Hello ~a, my name is ~a and I've greeted ~a times!" 
                your-name my-name greet-count))]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    greet-count]))

(define buggy-gobliny-alice
  (a-vat 'spawn ^buggy-memory-friend "Alice"))
(define buggy-imperative-alice
  (a-vat 'spawn ^buggy-imperative-friend "Alice"))}|

(Observe the @racket[error] put after both of them changed
@id{greet-count} and @id{recent-friend}.)

Okay, first let's check the initial @id{greet-count}:

@interact[
(a-vat 'call buggy-gobliny-alice 'greet-count)
(a-vat 'call buggy-imperative-alice 'greet-count)]

So far, so good.
Now let's greet both of them:

@interact-errors[
(a-vat 'call buggy-gobliny-alice 'greet "Chris")
(a-vat 'call buggy-imperative-alice 'greet "Chris")]

Okay, so both of them threw the error.
But what do you think the result of =greet-count= will be now?

@interact[
(a-vat 'call buggy-gobliny-alice 'greet-count)
(a-vat 'call buggy-imperative-alice 'greet-count)]


Now this is definitely different!
In the goblin'y example, by using goblin objects/actors, unhandled
errors means that breakage is as if nothing ever occurred.
We can log the error, but we won't mess up the rest of the system.
With the imperative code which uses =set!=, the state of our system
could become unintentionally corrupted and inconsistent.

This is what we mean by Goblins being transactional: something that
goes wrong need not be "committed" to the current state.
This is important for systems like financial infrastructure.
It turns out it also opens us up, in general, to becoming
@link["https://dustycloud.org/blog/goblins-time-travel-micropreview/"]
     {time wizards}.
But more on that later.


@section{Message passing, promises, and multiple vats}

@subsection{The basics}

Remember simpler times, when friends mostly just greeted us hello?

@codeblock|{
(define (^friend bcom my-name)
  (lambda (your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (a-vat 'spawn ^friend "Alice"))}|

We could of course make another friend that talks to Alice.

@run-codeblock|{
(define (^calls-friend bcom our-name)
  (lambda (friend)
    (define what-my-friend-said
      ($ friend our-name))
    (displayln (format "<~a>: I called my friend, and they said:"
                       our-name))
    (displayln (format "   \"~a\"" what-my-friend-said))))

(define archie
  (a-vat 'spawn ^calls-friend "Archie"))}|

Now Archie can talk to Alice:

@interact[
(a-vat 'call archie alice)]

Both Alice and Archie live in @id{a-vat}.
But @id{a-vat} isn't the only vat in town.  One other such vat
is @id{b-vat}, where Bob lives:

@run-codeblock|{
(define b-vat
  (make-vat))

(define bob
  (b-vat 'spawn ^calls-friend "Bob"))}|

Obviously, since Bob is in @id{b-vat}, we bootstrap a message call to
Bob from @id{b-vat}.
But what do you think happens when Bob tries to call Alice?

@interact-errors[
(b-vat 'call bob alice)]

Oh no!
It looks like Bob can't call Alice since they live in different
places!
From Archie's perspective, Alice was "near", aka "in the same vat".
However from Bob's perspective Alice was "far", aka "in some other
vat that isn't the one I'm in".
This is a problem because using the @racket[$] operator performs a
@emph{synchronous} call, but it's only safe to do synchronous calls for
objects that are near each other (in the same vat).

Fortunately there's something we can do: we can send a message from
Bob to Alice.
But we've never seen message sending in Goblins before, so what is that?

To prototype this, let's use the @id{'run} method on @id{b-vat}.
Remember, we saw the @id{'run} method used before, where it looked like:

@interact[
(a-vat 'run
       (lambda ()
         (define alyssa
           (spawn ^friend "Alyssa"))
         ($ alyssa "Ben")))]

So @id{'run} is just a way to run some arbitrary code in an actor context.
That sounds good enough for playing around with sending messages.
We can send messages with @racket[<-] so let's try that:

@interact[
(b-vat 'run
       (lambda ()
         (<- alice "Brenda")))]

Ah ok... so what @racket[<-] returns is something called a "Promise" which
might eventually be resolved to something interesting.
We want some way to be able to pull out that interesting thing.
That's what @racket[on] is for: it resolves promises and pulls out their
resolved value:

@delayed-interact[
(b-vat 'run
       (lambda ()
         (on (<- alice "Brenda")
             (lambda (alice-says)
               (displayln (format "Got from Alice: ~a" alice-says))))))]

@racket[<-] works just fine with far references, but it also works
just fine with near references too!
So we can run the same code in a-vat (where Alice is "near") and it
works there too:

@delayed-interact[
(a-vat 'run
       (lambda ()
         (on (<- alice "Arthur")
             (lambda (alice-says)
               (displayln (format "Got from Alice: ~a" alice-says))))))]

So using @racket[on] and @racket[<-] seems to fit our needs.
@; @note{However, we're glossing over something... we're now in the land of
@; asynchronous communication.  In fact since this documentation you are
@; reading is generated from real code examples, we had to kludgily add a
@; delay into the code interpreter to render this section appropriately
@; so that we had a better chance of the display output lining up with our
@; representation of REPL evaluation!}
But what would have happened if Alice had thrown an error?
Indeed, if we remember earlier we made @id{buggy-gobliny-alice}
so we can test for that.
It turns out that on can take a @id{#:catch} argument:

@delayed-interact[
(b-vat 'run
       (lambda ()
         (on (<- buggy-gobliny-alice 'greet "Brenda")
             (lambda (alice-says)
               (displayln (format "Got from Alice: ~a" alice-says)))
             #:catch
             (lambda (err)
               (displayln "Tried to talk to Alice, got an error :(")))))]

Now this is a little bit confusing to read because we saw two separate
messages here... it's important to realize that due to the way our vat
is configured, the exception backtrace being printed out is coming
from @id{a-vat}, not from our code being evaluated in @id{b-vat}.
We could configure the @id{a-vat} loop to do something different when it
hits errors, but currently it prints exceptions so we can debug them.
Anyway, so that's helpful information, but actually the place we caught
the error in @emph{our} code above was in the @racket[lambda] right
after @id{#:catch}.
As we can see, it did catch the error and we used that as an opportunity
to print out a complaint.

So @racket[<-] makes a promise for us.
We don't always need a promise; sometimes we're just calling something
for its effects.
For instance we might have a parrot that we like to encourage to say
silly things, maybe on the screen or even out loud, but we don't care
much about the result.
In that case we can use @racket[<-np] which sends a message but with
"no promise":

@delayed-interact[
(define parrot
  (a-vat 'spawn
         (lambda (bcom)
           (lambda (phrase)
             (code:comment "Since we're using displayln, we're printing this phrase")
             (code:comment "rather than returning it as a string")
             (displayln (format "<parrot>: ~a... *SQWAK!*" phrase))))))
(b-vat 'run
       (lambda ()
         (<-np parrot "Polly wants a chiptune")))]

When we don't need a promise, @racket[<-np] is an optimization that saves us
from some promise overhead.
But in most of our code, @racket[<-] performs the more common case
of returning a promise.

Anyway, we should have enough information to make a better constructor
for friends who are far away.
Recall our definition of @id{^calls-friend}:

@codeblock|{
(define (^calls-friend bcom our-name)
  (lambda (friend)
    (define what-my-friend-said
      ($ friend our-name))
    (displayln (format "<~a>: I called my friend, and they said:"
                       our-name))
    (displayln (format "   \"~a\"" what-my-friend-said))))}|

.. we'll make a few changes and name our constructor =^messages-friend=:

@run-codeblock|{
(define (^messages-friend bcom our-name)
  (lambda (friend)
    (on (<- friend our-name)
        (lambda (what-my-friend-said)
          (displayln (format "<~a>: I messaged my friend, and they said:"
                             our-name))
          (displayln (format "   \"~a\"" what-my-friend-said)))
        #:catch
        (lambda (err)
          (displayln
           "I messaged my friend but they broke their response promise...")))))}|

(We even made it a bit more robust than our previous implementation
by handling errors!)

Now we can make a version of Bob that can do a better job of holding a
conversation with his far-away friend Alice:

@delayed-interact[
  (define bob2
    (b-vat 'spawn ^messages-friend "Bob"))
  (b-vat 'call bob2 alice)]

Much better!


@subsection{Making and resolving our own promises}

So we know that @racket{<-} can make promises, but it turns out we can
make promises ourselves:

@interact[
  (a-vat 'run spawn-promise-cons)]

As we can see, promises come in pairs: the promise object, which we
can listen to with @racket[on], and the resolver object, which lets us
fulfill or break a promise.

We can also use @racket[spawn-promise-pair] to spawn a promise
(TODO: add footnote about why we didn't earlier, multiple value return
not being allowed from actors currently), which returns multiple
values (which we can bind with @racket[define-values]).
We can then try resolving a promise with @racket[on]... but of course
we'll need to fulfill or break it to see anything.

@delayed-interact[
  (a-vat 'run
         (lambda ()
           (define-values (foo-vow foo-resolver)
             (spawn-promise-pair))
           (define-values (bar-vow bar-resolver)
             (spawn-promise-pair))
           (define (declare-resolved result)
             (printf "Resolved: ~a\n" result))
           (define (declare-broken err)
             (printf "Broken: ~a\n" err))
           (on foo-vow
               declare-resolved
               #:catch declare-broken)
           (on bar-vow
               declare-resolved
               #:catch declare-broken)
           ($ foo-resolver 'fulfill 'yeah-foo)
           ($ bar-resolver 'break 'oh-no-bar)))]

By the way, you may notice that there's a naming convention in Goblins
(borrowed from E) to append a @id{-vow} suffix if something is a promise
(or should be treated as one).
That's a good practice for you to adopt, too.

@subsection{Finally we have #:finally}

Maybe we'd like to run something once a promise resolves, regardless
of whether or not it succeds or fails.
In such a case we can use the @id{#:finally} keyword:

@delayed-interact-errors[
  (a-vat 'run
         (lambda ()
           (define resolves-ok
             (spawn (lambda (bcom)
                      (lambda ()
                        "This is fine!"))))
           (define errors-out
             (spawn (lambda (bcom)
                      (lambda ()
                        (error "I am error!")))))
           (define (handle-it from-name vow)
             (on vow
                 (lambda (val)
                   (displayln
                    (format "Got from ~a: ~a" from-name val)))
                 #:catch
                 (lambda (err)
                   (displayln
                    (format "Error from ~a: ~a" from-name err)))
                 #:finally
                 (lambda ()
                   (displayln
                    (format "Done handling ~a." from-name)))))
           (handle-it 'resolves-ok (<- resolves-ok))
           (handle-it 'errors-out (<- errors-out))))]

@subsection{The on-fulfilled handler of "on" is optional}

Maybe all you care about is the @id{#:catch} or @id{#:finally} clause.
The @id{on-fulfilled} argument (ie, the first positional argument) to
@racket[on] is actually optional:

@delayed-interact-errors[
(define ((^throws-error bcom))
  (error "oh no"))
(define throws-error
  (a-vat 'spawn ^throws-error))
(a-vat 'run
       (lambda ()
         (on (<- throws-error)
             #:catch
             (lambda (err)
               (displayln (format "The error is: ~a" err))))))]

(Side note, this is the first time we've seen the procedure definition
style used in @id{^throws-error}... it's a way in Racket of making a
procedure that defines a procedure.  Handy in Goblins!  If you're new
to that, these two definitions are equivalent:)

@codeblock|{
  (define (^friend bcom my-name)
    (lambda (your-name)
      (format "Hello ~a, my name is ~a!" your-name my-name)))

  (define ((^friend bcom my-name) your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name))}|


@subsection{"on" with non-promise values}

@racket[on] works just fine if you pass in a non-promise value.
It'll just treat that value as if it were a promise that had
resolved immediately.
For example:

@delayed-interact[
(a-vat 'run
       (lambda ()
         (on 5
             (lambda (v)
               (displayln (format "Got: ~a" v))))))]

@subsection{"on" can return promises too}

It turns out that @racket[on] can also return a promise!
However, this isn't a very common use case, so you have to ask for it
via the @id{#:promise?} keyword.

@delayed-interact[
(define ((^bakery bcom name) carb)
  (format "~a's signature ~a baking" name carb))
(define petite-oven-bakery
  (a-vat 'spawn ^bakery "The Petite Oven"))
(a-vat 'run
       (lambda ()
         (define smell-vow
           (on (<- petite-oven-bakery "croissants")
               (lambda (what-you-smell)
                 (format "You smell ~a.  Heavenly!!"
                         what-you-smell))
               #:promise? #t))
         (on smell-vow displayln)))]

The choice of whether or not to include @id{#:catch} in an @racket[on]
with @id{#:promise?} affects whether or how an error will propagate
(or be cleaned up).

Without catching an error:

@delayed-interact-errors[
(define ((^throws-error bcom))
  (error "oh no"))
(define throws-error
  (a-vat 'spawn ^throws-error))
(a-vat 'run
       (lambda ()
         (on (on (<- throws-error)
                 (lambda (val)
                   (displayln "I won't run..."))
                 #:promise? #t)
             #:catch
             (lambda (e)
               (displayln (format "Caught: ~a" e))))))]

However if we catch the error, we can return a value that will succeed
if we like:

@delayed-interact-errors[
(a-vat 'run
       (lambda ()
         (on (on (<- throws-error)
                 #:catch
                 (lambda (e)
                   "An error?  Psh... this is fine.")
                 #:promise? #t)
             (lambda (val)
               (displayln (format "Got: ~a" val))))))]

But if our @racket{#:catch} handler raises an error, that error will
simply propagate (instead of the original one):

@delayed-interact-errors[
  (a-vat 'run
         (lambda ()
           (on (on (<- throws-error)
                   #:catch
                   (lambda (e)
                     (error "AAAAH!  This is NOT fine!"))
                   #:promise? #t)
               #:catch
               (lambda (e)
                 (displayln (format "Caught: ~a" e))))))]


@subsection{Promise pipelining}

@centered{
  "Machines grow faster and memories grow larger.
  But the speed of light is constant and New York is not getting any
  closer to Tokyo."

  @emph{from
        @link["http://www.erights.org/talks/thesis/"]{
              Robust Composition: Towards a Unified Approach
              to Access Control and Concurrency Control}
        by Mark S. Miller}}

Let's say we have a car factory that makes cars:

@run-codeblock|{
(define (^car-factory bcom company-name)
  (define ((^car bcom model color))
    (format "*Vroom vroom!*  You drive your ~a ~a ~a!"
            color company-name model))
  (define (make-car model color)
    (spawn ^car model color))
  make-car)

(define fork-motors
  (a-vat 'spawn ^car-factory "Fork"))}|

Now observe... in this scenario, Fork Motors exists on @id{a-vat}.
It will also generate a car that technically lives on @id{a-vat}.
Let's say we live on @id{b-vat}... we'd still like to drive our car.
Doing so seems extremely ugly:

@delayed-interact[
  (b-vat 'run
         (lambda ()
           (on (<- fork-motors "Explorist" "blue")
               (lambda (our-car)
                 (on (<- our-car)
                     displayln)))))]

This is hard to follow.
Maybe if we actually name some of those promises it'll be a bit easier
to read:

@delayed-interact[
  (b-vat 'run
         (lambda ()
           (define car-vow
             (<- fork-motors "Explorist" "blue"))
           (on car-vow
               (lambda (our-car)
                 (define drive-noise-vow
                   (<- our-car))
                 (on drive-noise-vow
                     displayln)))))]

Hm, not so much better.
Naming things helped us remember what each promise was for, but it
seems to be the nesting of @racket[on] handlers that's confusing.

Fortunately, Goblins supports something called "promise pipelining".
We can send an instruction to drive our car... before it even rolls
off the factory lot!
That's right... you can send messages to promises, before they have
even resolved!

@delayed-interact[
  (b-vat 'run
         (lambda ()
           (define car-vow
             (<- fork-motors "Explorist" "blue"))
           (define drive-noise-vow
             (<- car-vow))
           (on drive-noise-vow
               displayln)))]

Wow... that's *much* easier to read!

But readability is not the only goal of promise pipelining.
Like many things in Goblins,
@link["http://erights.org/elib/distrib/pipeline.html"]{
  promise pipelining comes from the E programming language}.
We're still working on distributed networked Goblins code, but the
foundations are there to do the same thing that E does: send messages
with less round trips!

Think about it this way: if @id{a-vat} actually lived on a different
server from @id{b-vat}, and both servers lived halfway across the globe
with the first way we implemented things:

@itemize[
  @item{@id{b-vat} would have to first send a message to the factory
        on @id{a-vat} first asking to make a car}
  @item{then @id{a-vat} would have to respond, resolving the promise
        with the location of the new car}
  @item{then @id{b-vat} would have to send @emph{another} message
        asking to drive the car}
  @item{then @id{a-vat} would have to respond, resolving yet another
        promise with the noise the car makes}
  @item{and finally =b-vat= can now display the car noise to the user.}]

With promise pipelining, this merely becomes:

@itemize[
  @item{@id{b-vat} would send a message to the factory on @id{a-vat}
        first asking to make a car and /at the same time/ can say,
        "and once this car is made, I'd like to send another message
        to it so I can drive it."}
  @item{then =a-vat= can make the car, drive the car, and then respond
        with the noise the car makes}
  @item{and now =b-vat= can display the car noise to the user.}]

Instead of going @id{B => A => B => A => B}, we have reduced our work to
@id{B => A => B} ... a significant savings in round-trips.
This can really add up in distributed applications, where (as Miller's
quote at the top of this subsection indicates) we may be limited in
how much we can prevent network delays by physics itself.


@subsection{Broken promise contagion}

Of course, now that we know that we can pipe promises together, what
happens if an error happens in the middle of the pipeline?

@run-codeblock|{
(define (^lessgood-car-factory bcom company-name)
  (define ((^car bcom model color))
    (format "*Vroom vroom!*  You drive your ~a ~a ~a!"
            color company-name model))
  (define (make-car model color)
    (error "Your car exploded on the factory floor!  Ooops!")
    (spawn ^car model color))
  make-car)

(define forked-motors
  (a-vat 'spawn ^lessgood-car-factory "Forked"))}|

The answer is that the error is simply propagated to all pending promises:

@delayed-interact-errors[
  (b-vat 'run
         (lambda ()
           (define car-vow
             (<- forked-motors "Exploder" "red"))
           (define drive-noise-vow
             (<- car-vow))
           (on drive-noise-vow
               displayln
               #:catch
               (lambda (err)
                 (displayln (format "Caught: ~a" err))))))]

@section{Going low-level: actormaps}

@subsection{Spawning, peeking, poking, turning}

So far we have dealt with vats, but there is a lower level of
abstraction in Goblins which is called an "actormap".

The key differences are:

@itemize[
  @item{
    @bold{vat}: An event loop implementation that runs continuously
    in its own thread.
    Comes pre-built with tooling to handle message passing between
    vats.
    Actually wraps an actormap.}
  @item{
    @bold{actormap}: The transactional datastructure that vats use.
    But, useful on its own to either build your own event loop
    or as a synchronous one-turn-at-a-time mapping of actor references
    to their current implementations.}]

The best way to learn is by doing, so let's make an actormap now:

@run-codeblock|{
(define am (make-actormap))}|

Let's add an actor to it.
Since cells are simple, let's add one of those.

@run-codeblock|{
(define lunchbox
  (actormap-spawn! am ^cell))}|

What's in the lunchbox?
We could take a look with @racket[actormap-peek]:

@interact[
  (actormap-peek am lunchbox)]

An empty lunchbox... we'd like to eat something later, so how about we
pack ourselves a nice sandwich by using @racket[actormap-poke]:

@interact[
  (actormap-poke! am lunchbox 'peanut-butter-and-jelly)
  (actormap-peek am lunchbox)]

What if we tried to look inside the lunchbox with @racket[actormap-poke!]
instead of @racket[actormap-peek]?

@interact[
  (actormap-poke! am lunchbox 'bbq-tofu)
  (actormap-poke! am lunchbox)]

Well that worked just fine.
What happens if we do both operations with @racket[actormap-peek]?

@interact[
  (actormap-peek am lunchbox 'chickpea-salad)
  (actormap-peek am lunchbox)]

What the...???
Our lunchbox didn't change!

We said that actormaps were transactional.
We have gotten a hint of this with the above:
@racket[actormap-poke!] returns a value and commits any changes.
@racket[actormap-peek] returns a value and throws any changes away.

Actually, these two functions are just simple conveniences that wrap a
more powerful (but cumbersome to use) tool, @racket[actormap-turn]:

@interact[
  (actormap-turn am lunchbox)
  (actormap-turn am lunchbox 'chickpea-salad)]

@racket[actormap-turn] returns four values to its continuation: a
return value (if any... Racket's REPL doesn't print out a
@racket[(void)], which is what actually got returned in the second
invocation), a "transactormap", and two lists representing messages
queued for delivery.
(*TODO:* Currently two, one for local and one for remote vats... we are
likely to collapse down to just one list, so update this text when
that changes!)

This @id{#<transactormap>} is interesting... we haven't mentioned this
before, but there are really two kinds of actormaps, "whactormaps"
(weak-hash actormaps) and "transactormaps" (which hold a transaction
which we can choose whether or not to commit).
Actually our @id{am} is a whactormap:

@interact[am]

It's a weak hashtable which stores the current mutable state of all
actors.

But we'd like to see about that transactormap... just running
@racket[actormap-turn] on its own won't update the lunchbox either:

@interact[
  (actormap-turn am lunchbox 'chickpea-salad)
  (actormap-turn am lunchbox)]

We need to capture the transactormap and commit it.

@interact[
  (define-values (_val tr-am _tl _tr)
    (actormap-turn am lunchbox 'chickpea-salad))
  tr-am
  (transactormap-merge! tr-am)]

Now at least our @id{am} actormap should reflect that we've switched out
our @id{'bbq-tofu} sandwich for a @id{'chickpea-salad} one:

@interact[
  (actormap-peek am lunchbox)]

Horray!


@subsection{Time travel: snapshotting and restoring}

But what does our actormap really look like?
Right now it only has one thing in it, the lunchbox cell.
We can see this by "snapshotting" the actormap.

@interact[
  (snapshot-whactormap am)]

This returns an immutable actormap, frozen in time.
We can see that it only has one pairing in it, and yep... it looks
like the key is our @id{lunchbox} reference:

@interact[lunchbox]

So it seems our "refr" is just a "reference"... it isn't the state of
the object itself, it's something that points to that state, through
an indirection of the actormap.

We now know that we can snapshot an actormap, we can freeze time and
come back to it.
First we'll freeze a snapshot of this period of time, change what's
in the lunchbox, and then snapshot it again:

@interact[
  (define am-snapshot1
    (snapshot-whactormap am))
  (actormap-poke! am lunchbox
                  (code:comment "sloppy joe w/ lentils")
                  'sloppy-jane)
  (define am-snapshot2
    (snapshot-whactormap am))]

Now let's restore them as separate whactormaps:

@interact[
  (define am-restored1
    (hasheq->whactormap am-snapshot1))
  (define am-restored2
    (hasheq->whactormap am-snapshot2))]

Now it doesn't matter what we put in our lunchbox; we can always
come back to life (and lunch) as it used to be.

@interact[
  (actormap-poke! am lunchbox 'peanut-butter-and-pickles)
  (actormap-peek am lunchbox)
  (actormap-peek am-restored2 lunchbox)
  (actormap-peek am-restored1 lunchbox)]

Why eat leftovers when you can simply travel back in time and eat
yesterday's lunch?


@section{Advanced object patterns}

@subsection{Extending our methods}

@subsection{Mixins}

@subsection{Behavior pivoting as a kind of state machine}

@section{How this relates to "object capability security"}

@section{Sealers/unsealers and rights amplification}

@section{Networked object interactions}

@section{A peek into Goblins' machinery}

@subsection{Actors really are what they say they are}

Now at last we will pull the curtain aside and see that the (hu?)man
standing behind the curtain is who we thought it was all along.
(This particular section is totally "optional", but hopefully
illuminates some of the guts of Goblins itself.)

Here is a suitable test subject:

@run-codeblock|{
(define (^the-wizard bcom)
  ;; High and mighty
  (define the-wizard-behind-the-curtain
    (methods
     [(request-gift what)
      "HOW DARE YOU APPROACH THE ALL POWERFUL WIZARD?"]
     [(pull-back-curtain)
      (bcom the-man
            "Er... I can explain...")]))
  ;; Just a person after all
  (define the-man
    (methods
     [(request-gift what)
      (match what
        ['heart
         "Here's a clock shaped like a heart!"]
        ['brain
         "Here's a diploma!"]
        ['courage
         "The courage was inside you all along!"]
        [anything-else
         "I don't know what that is..."])]
     [(pull-back-curtain)
      "... you already pulled it back."]))
  ;; we start out obscured
  the-wizard-behind-the-curtain)

(define wizard
  (actormap-spawn! am ^the-wizard))}|

We humbly approach the wizard asking for the gift of a heart:

@interact[
  (actormap-peek am wizard 'request-gift 'heart)]

Is that so?
Let's snapshot time both before and after we pull back the curtain
so we can remember how he changes his tone:

@interact[
  (define am-wizard-snapshot1
    (snapshot-whactormap am))
  (actormap-poke! am wizard 'pull-back-curtain)
  (define am-wizard-snapshot2
    (snapshot-whactormap am))]

Of course now, if we request a gift of the wizard, the conversation is
a bit different:

@interact[
  (actormap-peek am wizard 'request-gift 'heart)
  (actormap-peek am wizard 'request-gift 'brain)
  (actormap-peek am wizard 'request-gift 'courage)]

Of course, this isn't too much of a surprise, since, well, we wrote
the code for the wizard.

Both actormaps show our new wizard resident (and the old lunchbox
cell):

@interact[
  am-wizard-snapshot1
  am-wizard-snapshot2]

Referencing the wizard from either hashtable gives us this
weird-looking "ephemeron" thing:

@interact[
  (hash-ref am-wizard-snapshot1 wizard)]

Well, an
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") "ephemeron"]
is just
@link["https://en.wikipedia.org/wiki/Ephemeron"]{a datastructure}
to help the garbage collection in the weak hashtable of whactormaps
work right.  So we can peel that off:

@interact[
  (ephemeron-value
   (hash-ref am-wizard-snapshot1 wizard))]

What's a... "mactor"?
Well, it stands for "meta-actor"... there are a few of these
kinds of things.
A @id{mactor:local-actor} is one kind of mactor (actually the most common
kind), one that works in the usual way of wrapping a procedure.

It would be wonderful to be able to look at that procedure.
Unfortunately, we haven't pulled in all the tools to do it yet, but
there is a way to do so:

@interact[
  (require (submod goblins/core mactor-extra))]

This gives us access to @racket[mactor:local-actor-handler], which is what
we want.

@interact[
  (mactor:local-actor-handler
   (ephemeron-value
    (hash-ref am-wizard-snapshot1 wizard)))]

Let's give names to both snapshotted wizard handlers:

@interact[
  (define wizard-handler1
    (mactor:local-actor-handler
     (ephemeron-value
      (hash-ref am-wizard-snapshot1 wizard))))
  (define wizard-handler2
    (mactor:local-actor-handler
     (ephemeron-value
      (hash-ref am-wizard-snapshot2 wizard))))]

Since these are just procedures, we can try calling them directly.
As we can see, the first wizard is petulant in response to our request
for a gift, whereas the second wizard, having had the curtain pulled
away, is happy to assist.

@interact[
  (wizard-handler1 'request-gift 'heart)
  (wizard-handler2 'request-gift 'heart)]

So this really was the procedure we thought it was!

But wait... if we remember correctly, our first wizard version,
@id{the-wizard-behind-the-curtain}, returned its request to "become"
something (as well as its return value) wrapped by its =bcom=
capability when we called the @id{'pull-back-curtain} method.
So what happens if we try calling that again on the unwrapped
procedure?

@interact[
  (wizard-handler1 'pull-back-curtain)]

That's interesting... it returned a "become" object!
How can we unwrap it?

Well, here's the secret about @id{bcom}: as we said, it is a capability,
but it's actually using a "sealer" (we'll get to what those are later,
which as it sounds like is a kind of capability to "seal" an object in
a certain way).
To unseal it, we need the corresponding "unsealer".
@id{mactor:local-actor} has a way of detecting that the value is wrapped
in this sealer, as well as a way of unsealing it:

@interact[
  (define wizard-become?
    (mactor:local-actor-become?
     (ephemeron-value
      (hash-ref am-wizard-snapshot1 wizard))))
  (define wizard-become-unsealer
    (mactor:local-actor-become-unsealer
     (ephemeron-value
      (hash-ref am-wizard-snapshot1 wizard))))]

Now we can see that the value returned from the ='pull-back-curtain=
method is from the wizard's @id{bcom} sealer:

@interact[
  (wizard-become? (wizard-handler1 'pull-back-curtain))]

And now, at last!
We can peer into its contents:

@interact[
  (wizard-become-unsealer (wizard-handler1 'pull-back-curtain))]

Aha!
It looks like it returns two values to its continuation...
the procedure that the actor would like to become, as well as
an extra (and optional, defaulting to @racket[(void)]) return value.
Let's bind the first of those to a useful name:

@interact[
  (define-values (new-wizard-handler _wizval)
    (wizard-become-unsealer (wizard-handler1 'pull-back-curtain)))]

Now as you've probably guessed, this ought to be the same procedure as
@id{wizard-handler2}.
Is it?

@interact[
  (code:comment "Works just like wizard-handler2...")
  (new-wizard-handler 'request-gift 'courage)
  (code:comment "... because it *is* wizard-handler2!")
  (eq? new-wizard-handler wizard-handler2)]

Woohoo!

Of course, the tools we've seen in this subsection are not really
tools that we expect users of Goblins to use very regularly.
(Indeed, when we implement better module-level security, rarely will
they have access to them.)
But now we've gotten a peek inside of Goblins' machinery, and seen
that it was mostly what we expected all along.

I feel like all that learning is was worth a reward, don't you?

@interact[
  (new-wizard-handler 'request-gift 'brain)]

Yes, we deserve it!


@subsection{So how does message passing work}

@subsection{How can two actormaps communicate?  (How do vats do it?)}

