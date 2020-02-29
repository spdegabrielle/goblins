#lang scribble/manual

@title{What is Goblins?}

Goblins is a quasi-functional distributed object system.
Its design allows for object-capability security, allowing for safe
distributed programming environments.
Its design is inspired by the
@link["http://www.erights.org/"]{E programming language}
and by the observation that
@link["http://mumble.net/~jar/pubs/secureos/secureos.html"]
     {lambda is already the ultimate security mechanism}
(ie, normal argument-passing in programs, if taken seriously/purely,
is already all the security system we need).

@section{What works so far?}

@itemlist[
  @item{@bold{Quasi-functional object system:}
        Users hold on to references of objects/actors, but objects/actors
        are just procedures.
        Rather than directly mutating, objects/actors can specify that
        they should "become" a new version of themselves when they
        handle the next invocation.}
  @item{@bold{Transactional updates:}
        Changes happen within a transaction.
        If an unhandled exception occurs, we can "roll back" history
        as if the message never happened, avoiding confused state
        changes throughout the system.}
  @item{@bold{Time travel:}
        We can snapshot old revisions of the system and
        interact with them.}
  @item{@bold{Asynchronous programming with sophisticated promise chaining:}
        Asynchronous message passing with promises is supported.
        Promises work such that there's no need to wait for promises
        to resolve before interacting... you can communicate with the
        future that doesn't even yet exist!
        Sent a message to that remote car factory asking for a new car?
        Why wait for the car to be delivered... you can send it a
        drive message at the same time, and both messages can be
        delivered across the network in a single hop.}
  @item{@bold{Communicating event loops:}
        Objects across multiple event loops (aka "vats") can
        communicate, whether in the same OS process or (soon) across
        the network.}
  @item{@bold{Synchronous and asynchronous behavior, integrated but distinguished*:}
        Both synchronous and asynchronous programming is supported,
        but only objects in the same "vat" (event loop) can perform
        synchronous immediate calls.
        All objects can perform asynchronous calls against each other.}
  @item{@bold{A library, not a language:}
        Goblins is itself a library that can be utilized with nearly
        any Racket program, including many Racket #langs.
        (However, some languages may be provided or encouraged for additional
        security / convenience later).}
  @item{@bold{Object capability security:}
        Goblins itself is built for object capability (ocap) security,
        which is to say that you can only operate on the references
        you have access to.
        Goblins embraces and builds upon this foundation.
        (However, full ocap security will require a safer module
        system than the one Racket provides; coming eventually.)}]

@section{What's on its way?}

@itemlist[
  @item{@bold{Fully distributed, networked, secure p2p communication:}
        In the future, it will be possible to communicate with other
        objects over a network connection.
        Distributed object interactions is safe because of ocap security
        guarantees.}
  @item{@bold{A separate module system:}
        While not part of Goblins itself, a future project named
        "Spritely Dungeon" will help close the loop on ocap security for
        Racket.}]
       
