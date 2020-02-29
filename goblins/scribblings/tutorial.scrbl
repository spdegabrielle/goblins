#lang scribble/manual

@(require scriblib/footnote
          (for-label racket goblins)
          "eval.rkt")

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

Here the arguments to the spawn method are @racketidfont{^friend},
which is the constructor procedure we are using, and the argument
"Alice", which becomes bound to @racketidfont{my-name}.
(The @racketidfont{bcom} argument is implicitly provided by Goblins;
we'll ignore it for right now.)

@run-codeblock|{
  (define foo 'bar)}|
