#lang racket/base

;;; Exports
;;; =======

(require (for-syntax syntax/parse racket/base)
         racket/contract
         racket/set
         "utils/simple-sealers.rkt")

;; Refrs
;; Same as E refs, but -ref is already meaningful in scheme
(provide live-refr?
         sturdy-refr?
         local-refr?
         remote-refr?

         ;; From an external user perspective, most users won't think
         ;; of these much as being refrs, but conceptually as if they
         ;; are actually objects and promises.
         (rename-out [local-object-refr? local-object?])
         (rename-out [local-promise-refr? local-promise?])
         (rename-out [remote-object-refr? remote-object?])
         (rename-out [remote-promise-refr? remote-promise?])

         near-refr?
         far-refr?)

;; The making-and-modifying actormap functions
(provide make-whactormap
         ;; alias of make-actormap
         (rename-out [make-whactormap make-actormap])
         whactormap?

         snapshot-whactormap hasheq->whactormap

         make-transactormap
         transactormap?

         transactormap-parent
         transactormap-delta
         transactormap-merged?
         transactormap-merge!

         actormap?

         syscaller-free-thread)

;; Not sure if there's any need to export this, but...
(module+ actormap-extra
  (provide whactormap-ref
           whactormap-set!

           actormap-ref
           actormap-set!

           transactormap-set! transactormap-ref

           actormap-vat-connector))

;; The operating-on-actormap main functions
(provide actormap-turn
         actormap-poke!
         actormap-reckless-poke!
         actormap-peek
         actormap-turn-message

         actormap-run
         actormap-run*
         actormap-run!

         ;; preventing awkwardness: we don't want actor handlers
         ;; to be actor refrs
         ;; TODO: But is this a problem?  Vats are accessing these procedures
         ;;   through these contracts, which almost certainly add slowdown.
         ;;   But the Vat code is more or less "trusted" to be have right.
         actormap-spawn
         actormap-spawn!

         call $  ; $ is an alias
         spawn

         define-spawned

         on listen
         <-np <-

         spawn-proc spawn-const

         spawn-promise-values spawn-promise-cons)


(module+ debug
  (provide whactormap-wht))

;; meh...
(module+ for-vats
  (provide local-refr-vat-connector)
  (provide remote-refr-captp-connector))

;;; Imports
;;; =======

(require "message.rkt"
         "hash-contracts.rkt"
         racket/match
         racket/generic)



;;;                  .============================.
;;;                  | High level view of Goblins |
;;;                  '============================'
;;;
;;; There's a lot of architecture here.  core.rkt is most of the heart
;;; of the system (though vat.rkt and captp.rkt fill in the rest.)
;;; It's a lot to take in, so let's start out with a "high level view".
;;; Here's an image to get started:
;;;
;;;   .----------------------------------.         .-------------------.
;;;   |            Machine 1             |         |     Machine 2     |
;;;   |            =========             |         |     =========     |
;;;   |                                  |         |                   |
;;;   | .--------------.  .---------.   .-.       .-.                  |
;;;   | |    Vat A     |  |  Vat B  |   |  \______|  \_   .----------. |
;;;   | |  .---.       |  |   .-.   | .-|  /      |  / |  |   Vat C  | |
;;;   | | (Alice)----------->(Bob)----' '-'       '-'  |  |  .---.   | |
;;;   | |  '---'       |  |   '-'   |    |         |   '--->(Carol)  | |
;;;   | |      \       |  '----^----'    |         |      |  '---'   | |
;;;   | |       V      |       |         |         |      |          | |
;;;   | |      .----.  |       |        .-.       .-.     |  .----.  | |
;;;   | |     (Alfred) |       '-------/  |______/  |____---(Carlos) | |
;;;   | |      '----'  |               \  |      \  |     |  '----'  | |
;;;   | |              |                '-'       '-'     '----------' |
;;;   | '--------------'                 |         |                   |
;;;   |                                  |         |                   |
;;;   '----------------------------------'         '-------------------'
;;;
;;; Here we see the following:
;;;
;;;  - Zooming in the farthest, we are looking at the "object layer"...
;;;    Alice has a reference to Alfred and Bob, Bob has a reference to Carol,
;;;    Carlos has a reference to Bob.  Reference possession is directional;
;;;    even though Alice has a reference to Bob, Bob does not have a
;;;    reference to Alice.
;;;
;;;  - One layer up is the "vat layer"... here we can see that Alice and
;;;    Alfred are both objects in Vat A, Bob is an object in Vat B, and
;;;    Carol and Carlos are objects in Vat C.
;;;
;;;  - Zooming out the farthest is the "machine/network level".
;;;    There are two machines (Machine 1 and Machine 2) connected over a
;;;    Goblins CapTP network.  The stubby shapes on the borders between the
;;;    machines represent the directions of references Machine 1 has to
;;;    objects in Machine 2 (at the top) and references Machine 2 has to
;;;    Machine 1.  Both machines in this diagram are cooperating to preserve
;;;    that Bob has access to Carol but that Carol does not have access to
;;;    Bob, and that Carlos has access to Bob but Bob does not have access
;;;    to Carlos.  (However there is no strict guarantee from either
;;;    machine's perspective that this is the case... generally it's in
;;;    everyone's best interests to take a "principle of least authority"
;;;    approach though so usually it is.)
;;;
;;; This illustration is what's sometimes called a "grannovetter diagram"
;;; in the ocap community, inspired by the kinds of diagrams in Mark
;;; S. Grannovetter's "The Strength of Weak Ties" paper.  The connection is
;;; that while the "Weak Ties" paper was describing the kinds of social
;;; connections between people (Alice knows Bob, Bob knows Carol), similar
;;; patterns arise in ocap systems (the object Alice has a refernce to Bob,
;;; and Bob has a reference to Carol).
;;;
;;; With that in mind, we're now ready to look at things more structurally.
;;;
;;;
;;;                  .============================.
;;;                  | Goblins abstraction layers |
;;;                  '============================'
;;;
;;; Generally, things look like so:
;;;
;;;   (machine (vat (actormap {refr: (mactor object-handler)})))
;;;
;;; However, we could really benefit from looking at those in more detail,
;;; so from the outermost layer in...
;;;
;;;    .--- A machine in Goblins is basically an OS process.
;;;    |    However, the broader Goblins CapTP/MachineTP network is
;;;    |    made up of many machines.  A connection to another machine
;;;    |    is the closest amount of "assurance" a Goblins machine has
;;;    |    that it is delivering to a specific destination.
;;;    |    Nonetheless, Goblins users generally operate at the object
;;;    |    reference level of abstraction, even across machines.
;;;    |
;;;    |    An object reference on the same machine is considered
;;;    |    "local" and an object reference on another machine is
;;;    |    considered "remote".
;;;    |
;;;    |      .--- Chris: "How about I call this 'hive'?"
;;;    |      |    Ocap community: "We hate that, use 'vat'"
;;;    |      |    Everyone else: "What's a 'vat' what a weird name"
;;;    |      |
;;;    |      |    A vat is a traditional ocap term, both a container for
;;;    |      |    objects but most importantly an event loop that
;;;    |      |    communicates with other event loops.  Vats operate
;;;    |      |    "one turn at a time"... a toplevel message is handled
;;;    |      |    for some object which is transactional; either it happens
;;;    |      |    or, if something bad happens in-between, no effects occur
;;;    |      |    at all (except that a promise waiting for the result of
;;;    |      |    this turn is broken).
;;;    |      |
;;;    |      |    Objects in the same vat are "near", whereas objects in
;;;    |      |    remote vats are "far".  (As you may notice, "near" objects
;;;    |      |    can be "near" or "far", but "remote" objects are always
;;;    |      |    "far".)
;;;    |      |
;;;    |      |    This distinction is important, because Goblins supports
;;;    |      |    both asynchronous messages + promises via `<-` and
;;;    |      |    classic synchronous call-and-return invocations via `$`.
;;;    |      |    However, while any actor can call any other actor via
;;;    |      |    <-, only near actors may use $ for synchronous call-retun
;;;    |      |    invocations.  In the general case, a turn starts by
;;;    |      |    delivering to an actor in some vat a message passed with <-,
;;;    |      |    but during that turn many other near actors may be called
;;;    |      |    with $.  For example, this allows for implementing transactional
;;;    |      |    actions as transferring money from one account/purse to another
;;;    |      |    with $ in the same vat very easily, while knowing that if
;;;    |      |    something bad happens in this transaction, no actor state
;;;    |      |    changes will be committed (though listeners waiting for
;;;    |      |    the result of its transaction will be informed of its failure);
;;;    |      |    ie, the financial system will not end up in a corrupt state.
;;;    |      |    In this example, it is possible for users all over the network
;;;    |      |    to hold and use purses in this vat, even though this vat is
;;;    |      |    responsible for money transfer between those purses.
;;;    |      |    For an example of such a financial system in E, see
;;;    |      |    "An Ode to the Grannovetter Diagram":
;;;    |      |      http://erights.org/elib/capability/ode/index.html
;;;    |      |
;;;    |      |    .--- Earlier we said that vats are both an event loop and
;;;    |      |    |    a container for storing actor state.  Surprise!  The
;;;    |      |    |    vat is actually wrapping the container, which is called
;;;    |      |    |    an "actormap".  While vats do not expose their actormaps,
;;;    |      |    |    Goblins has made a novel change by allowing actormaps to
;;;    |      |    |    be used as independent first-class objects.  Most users
;;;    |      |    |    will rarely do this, but first-class usage of actormaps
;;;    |      |    |    is still useful if integrating Goblins with an existing
;;;    |      |    |    event loop (such as one for a video game or a GUI) or for
;;;    |      |    |    writing unit tests.
;;;    |      |    |
;;;    |      |    |    The keys to actormaps are references (called "refrs")
;;;    |      |    |    and the values are current behavior.  This is described
;;;    |      |    |    below.
;;;    |      |    |
;;;    |      |    |    Actormaps also technically operate on "turns", which are
;;;    |      |    |    a transactional operation.  Once a turn begins, a dynamic
;;;    |      |    |    "syscaller" (or "actor context") is initialized so that
;;;    |      |    |    actors can make changes within this transaction.  At the
;;;    |      |    |    end of the turn, the user of actormap-turn is presented
;;;    |      |    |    with the transactional actormap (called "transactormap")
;;;    |      |    |    which can either be committed or not to the current mutable
;;;    |      |    |    actormap state ("whactormap", which stands for
;;;    |      |    |    "weak hash actormap"), alongside a queue of messages that
;;;    |      |    |    were scheduled to be run from actors in this turn using <-,
;;;    |      |    |    and the result of the computation run.
;;;    |      |    |
;;;    |      |    |    However, few users will operate using actormap-turn directly,
;;;    |      |    |    and will instead either use actormap-poke! (which automatically
;;;    |      |    |    commits the transaction if it succeeds or propagates the error)
;;;    |      |    |    or actormap-peek (which returns the result but throws away the
;;;    |      |    |    transaction; useful for getting a sense of what's going on
;;;    |      |    |    without committing any changes to actor state).
;;;    |      |    |    Or, even more commonly, they'll just use a vat and never think
;;;    |      |    |    about actormaps at all.
;;;    |      |    |
;;;    |      |    |         .--- A reference to an object or actor.
;;;    |      |    |         |    Traditionally called a "ref" by the ocap community, but
;;;    |      |    |         |    scheme already uses "-ref" everywhere so we call it
;;;    |      |    |         |    "refr" instead.  Whatever.
;;;    |      |    |         |
;;;    |      |    |         |    Anyway, these are the real "capabilities" of Goblins'
;;;    |      |    |         |    "object capability system".  Holding onto one gives you
;;;    |      |    |         |    authority to make invocations with <- or $, and can be
;;;    |      |    |         |    passed around to procedure or actor invocations.
;;;    |      |    |         |    Effectively the "moral equivalent" of a procedure
;;;    |      |    |         |    reference.  If you have it, you can use (and share) it;
;;;    |      |    |         |    if not, you can't.
;;;    |      |    |         |
;;;    |      |    |         |    Actually, technically these are local-live-refrs...
;;;    |      |    |         |    see "The World of Refrs" below for the rest of them.
;;;    |      |    |         |
;;;    |      |    |         |      .--- We're now at the "object behavior" side of
;;;    |      |    |         |      |    things.  I wish I could avoid talking about
;;;    |      |    |         |      |    "mactors" but we're talking about the actual
;;;    |      |    |         |      |    implementation here so... "mactor" stands for
;;;    |      |    |         |      |    "meta-actor", and really there are a few
;;;    |      |    |         |      |    "core kinds of behavior" (mainly for promises
;;;    |      |    |         |      |    vs object behavior).  But in the general case,
;;;    |      |    |         |      |    most objects from a user's perspective are the
;;;    |      |    |         |      |    mactor:object kind, which is just a wrapper
;;;    |      |    |         |      |    around the current object handler (as well as
;;;    |      |    |         |      |    some information to track when this object is
;;;    |      |    |         |      |    "becoming" another kind of object.
;;;    |      |    |         |      |
;;;    |      |    |         |      |      .--- Finally, "object"... a term that is
;;;    |      |    |         |      |      |    unambiguous and well-understood!  Well,
;;;    |      |    |         |      |      |    "object" in our system means "references
;;;    |      |    |         |      |      |    mapping to an encapsulation of state".
;;;    |      |    |         |      |      |    Refrs are the reference part, so
;;;    |      |    |         |      |      |    object-handlers are the "current state"
;;;    |      |    |         |      |      |    part.  The time when an object transitions
;;;    |      |    |         |      |      |    from "one" behavior to another is when it
;;;    |      |    |         |      |      |    returns a new handler wrapped in a "become"
;;;    |      |    |         |      |      |    wrapper specific to this object (and
;;;    |      |    |         |      |      |    provided to the object at construction
;;;    |      |    |         |      |      |    time)
;;;    |      |    |         |      |      |
;;;    V      V    V         V      V      V
;;; (machine (vat (actormap {refr: (mactor object-handler)})))
;;;
;;;
;;; Whew!  That's a lot of info, so go take a break and then we'll go onto
;;; the next section.
;;;
;;;
;;;                     .====================.
;;;                     | The World of Refrs |
;;;                     '===================='
;;;
;;; There are a few kinds of references, explained below:
;;;
;;;                                     live refrs :
;;;                     (runtime or captp session) : offline-storeable
;;;                     ========================== : =================
;;;                                                :
;;;                local?           remote?        :
;;;           .----------------.----------------.  :
;;;   object? | local-object   | remote-object  |  :    [sturdy refrs]
;;;           |----------------+----------------|  :
;;;  promise? | local-promise  | remote-promise |  :     [cert chains]
;;;           '----------------'----------------'  :
;;;
;;; On the left hand side we see live references (only valid within this
;;; process runtime or between machines across captp sessions) and
;;; offline-storeable references (sturdy refrs, a kind of bearer URI,
;;; and certificate chains, which are like "deeds" indicating that the
;;; possessor of some cryptographic material is permitted access).
;;;
;;; All offline-storeable references must first be converted to live
;;; references before they can be used (authority to do this itself a
;;; capability, as well as authority to produce these offline-storeable
;;; objects).
;;;
;;; Live references subdivide into local (on the same machine) and
;;; remote (on a foreign machine).  These are typed as either
;;; representing an object or a promise.
;;;
;;; (Local references also further subdivide into "near" and "far",
;;; but rather than being encoded in the reference type this is
;;; determined relative to another local-refr or the current actor
;;; context.)

;;; Refrs
;;; =====

(struct live-refr ())

(struct local-refr live-refr (vat-connector))

;; TODO: Should we shorten to just local-object and friends?
;;   Drop the -refr?
(struct local-object-refr local-refr (debug-name)
  #:constructor-name _make-local-object-refr
  #:methods gen:custom-write
  [(define (write-proc refr port mode)
     (define str-to-write
       (match (local-object-refr-debug-name refr)
         [#f "#<local-object>"]
         ;; TODO: Do we need to do escaping?
         [debug-name (format "#<local-object ~a>" debug-name)]))
     (write-string str-to-write port))])

(struct local-promise-refr local-refr ()
  #:constructor-name _make-local-promise-refr
  #:methods gen:custom-write
  [(define (write-proc refr port mode)
     (write-string "#<local-promise>" port))])

(define (make-local-object-refr [debug-name #f] [vat-connector #f])
  (_make-local-object-refr vat-connector debug-name))
(define (make-local-promise-refr [vat-connector #f])
  (_make-local-promise-refr vat-connector))

;; Captp-connector should be a procedure which both sends a message
;; to the local machine representative actor, but also has something
;; serialized that knows which specific remote machine + session this
;; corresponds to (to look up the right captp session and forward)
(struct remote-refr live-refr (captp-connector sealed-pos))

(struct remote-object-refr remote-refr ()
  #:constructor-name make-remote-object-refr
  #:methods gen:custom-write
  [(define (write-proc refr port mode)
     (write-string "#<remote-object>" port))])

(struct remote-promise-refr remote-refr ()
  #:constructor-name make-remote-promise-refr
  #:methods gen:custom-write
  [(define (write-proc refr port mode)
     (write-string "#<remote-promise>" port))])

(struct sturdy-refr (swiss-num vat-id conn-hints))


;;; Meta-actors and miranda methods
;;; ===============================

(module+ mactor-extra
  (provide mactor mactor?

           mactor:object mactor:object?
           mactor:object-handler
           mactor:object-become-unsealer
           mactor:object-become?

           mactor:eventual?
           mactor:eventual-resolver-unsealer
           mactor:eventual-resolver-tm?

           mactor:unresolved?
           mactor:unresolved-listeners

           mactor:naive mactor:naive?
           mactor:naive-waiting-messages

           mactor:question mactor:question?
           mactor:question-captp-connector
           mactor:question-question-finder

           mactor:closer mactor:closer?
           mactor:closer-point-to
           mactor:closer-history

           mactor:remote-link?
           mactor:remote-link-point-to

           mactor:local-link
           mactor:local-link-point-to

           mactor:encased mactor:encased?
           mactor:encased-val

           mactor:broken mactor:broken?
           mactor:broken-problem))

;;;                    .======================.
;;;                    | The World of Mactors |
;;;                    '======================'
;;;
;;; This is getting really deep into the weeds and is really only
;;; relevant to anyone hacking on this module.
;;;
;;; Mactors are only ever relevant to the internals of a vat, but they
;;; do define some common behaviors.
;;;
;;; Here are the categories and transition states:
;;;
;;;        Unresolved                     Resolved
;;;  __________________________  ___________________________
;;; |                          ||                           |
;;;
;;;                 .----------------->.    .-->[object]
;;;                 |                  |    |
;;;                 |    .--.          |    +-->[local-link]
;;;     [naive]-->. |    v  |          |    |            
;;;               +>+->[closer]------->'--->+-->[encased]
;;;  [question]-->' |       |               |            
;;;                 |       |               '-->[broken]
;;;                 '------>'--->[remote-link]    ^
;;;                                  |            |
;;;                                  '----------->'
;;;
;;; |________________________________________||_____________|
;;;                  Eventual                     Settled
;;;
;;; The four major categories of mactors:
;;;
;;;  - Unresolved: A promise that has never been fulfilled or broken.
;;;  - Resolved: Either an object with its own handler or a promise which
;;;    has been fulfilled to some value/object reference or which has broken.
;;; 
;;; and:
;;;
;;;  - Eventual: Something which *might* eventually transition its state.
;;;  - Settled: Something which will never transition its state again.
;;;
;;; The surprising thing here is that there is any distinction between
;;; unresolved/resolved and eventual/settled at all.  The key to
;;; understanding the difference is observing that a mactor:remote-link
;;; might become broken upon network disconnect from that object.
;;;
;;; One intersting observation is that if you have a local-object-refr that
;;; it is sure to correspond to a mactor:object.  A local-promise-refr can
;;; correspond to any object state *except* for mactor:object (if a promise
;;; resolves to a local object, it must point to it via mactor:local-link.)
;;; (remote-refrs of course never correspond to a mactor on this machine;
;;; those are managed by captp.)
;;;
;;; See also:
;;;  - The comments above each of these below
;;;  - "Miranda methods":
;;;      http://www.erights.org/elang/blocks/miranda.html
;;;  - "Reference mechanics":
;;;      http://erights.org/elib/concurrency/refmech.html

(struct mactor ())

;; local-objects are the most common type, have a message handler
;; which specifies how to respond to the next message, as well as
;; a predicate and unsealer to identify and unpack when a message
;; handler specifies that this actor would like to "become" a new
;; version of itself (get a new handler)
(struct mactor:object mactor
  (handler become-unsealer become?))

;; The other kinds of mactors correspond to promises and their resolutions.

;; There are two supertypes here which are not used directly:
;; mactor:unresolved and mactor:eventual.  See above for an explaination
;; of what these mean.
(struct mactor:eventual mactor
  ;; We can still be resolved, so identify who is allowed to do that
  (resolver-unsealer resolver-tm?))
(struct mactor:unresolved mactor:eventual
  ;; Who's listening for a resolution?
  (listeners))

;; The most common kind of freshly made promise is a naive one.
;; It knows no interesting information about how what it will eventually
;; become.
;; Since it knows of no closer information it keeps a queue of waiting
;; messages which will eventually be transmitted.
(struct mactor:naive mactor:unresolved
  (;; All of these get "rewritten" as this promise is either resolved
   ;; or moved closer to resolution.
   waiting-messages))

;; A special kind of "freshly made" promise which also corresponds to being
;; a question on the remote end.  Keeps track of the captp-connector
;; relevant to this connection so it can send it messages and the
;; question-finder that it corresponds to (used for passing along messages).
(struct mactor:question mactor:unresolved
  (captp-connector question-finder))

;; "You make me closer to God" -- Nine Inch Nails
;; Well, in this case we're actually just "closer to resolution"...
;; pointing at some other promise that isn't us.
(struct mactor:closer mactor:unresolved
  (;; Who do we currently point to?
   point-to
   ;; A set of promises we used to point to before they themselves
   ;; resolved... used to detect cycles
   history
   ;; Any messages that are waiting to be passed along...
   ;; Currently only if we're pointing to a remote-promise, otherwise
   ;; this will be an empty list.
   waiting-messages))

;; Point at a remote object.
;; It's eventual because, well, it could still break on network partition.
(struct mactor:remote-link mactor:eventual
  (point-to))

;; Link to an object on the same machine.
(struct mactor:local-link mactor
  (point-to))

;; A promise that has resolved to some value
(struct mactor:encased mactor
  (val))

;; Breakage (and remember why!)
(struct mactor:broken mactor
  (problem))

(define (mactor:unresolved-add-listener mactor new-listener)
  (match mactor
    [(mactor:naive resolver-unsealer resolver-tm? listeners
                   waiting-messages)
     (mactor:naive resolver-unsealer resolver-tm?
                   (cons new-listener listeners)
                   waiting-messages)]
    [(mactor:question resolver-unsealer resolver-tm? listeners
                      captp-connector question-finder)
     (mactor:question resolver-unsealer resolver-tm?
                      (cons new-listener listeners)
                      captp-connector question-finder)]
    [(mactor:closer resolver-unsealer resolver-tm? listeners
                    point-to history waiting-messages)
     (mactor:closer resolver-unsealer resolver-tm?
                    (cons new-listener listeners)
                    point-to history waiting-messages)]))

;; Helper for syscaller's fulfill-promise and break-promise methods
(define (unseal-mactor-resolution mactor sealed-resolution)
  (define resolver-tm?
    (mactor:eventual-resolver-tm? mactor))
  (define resolver-unsealer
    (mactor:eventual-resolver-unsealer mactor))
  ;; Is this a valid resolution?
  (unless (resolver-tm? sealed-resolution)
    (error "Resolution sealed with wrong trademark!"))
  (resolver-unsealer sealed-resolution))

(define (near-refr? refr)
  ((current-syscaller) 'near-refr? refr))
(define (far-refr? refr)
  (not (near-refr? refr)))


;;; "Become" special sealers
;;; ========================

;; Note that this isn't really perfect; if someone has intercepted an
;; old become-value, they can still make us become that again... but
;; that's a fairly rare risk probably (I can't think of any likely
;; scenarios currently)

(define (make-become-sealer-triplet)
  (define-values (struct:seal make-seal sealed? seal-ref seal-set!)
    (make-struct-type 'become #f 2 0))
  (define (become new-handler [return-val (void)])
    (make-seal new-handler return-val))
  (define unseal-become-handler
    (procedure-rename
     (make-struct-field-accessor seal-ref 0)
     'unseal-become-handler))
  (define unseal-become-return-val
    (procedure-rename
     (make-struct-field-accessor seal-ref 1)
     'unseal-become-return-val))
  (define (unseal sealed)
    (values (unseal-become-handler sealed)
            (unseal-become-return-val sealed)))
  (values become unseal sealed?))

;;; Actormaps, whactormaps and transactormaps
;;; =========================================

;; An transactormap is a transactional structure used by the
;; actormap turn system

;; Uses ephemerons to allow for self-referencing collection...
;; hopefully works right.
(define-generics actormap
  (actormap-ref actormap key [dflt])
  (actormap-set! actormap key val)
  (actormap-vat-connector actormap))

(struct whactormap (wht            ; weak hash table
                    vat-connector) ; if we have a vat, here's how to talk to it
  #:methods gen:actormap
  [(define (actormap-ref whactormap key [dflt #f])
     (whactormap-ref whactormap key dflt))
   (define (actormap-set! whactormap key val)
     (whactormap-set! whactormap key val))
   (define (actormap-vat-connector whactormap)
     (whactormap-vat-connector whactormap))])

(define (make-whactormap #:vat-connector [vat-connector #f])
  (whactormap (make-weak-hasheq) vat-connector))
(define (whactormap-ref whactormap key [dflt #f])
  (define val
    (hash-ref (whactormap-wht whactormap) key #f))
  (or (and val (ephemeron-value val #f key))
      dflt))

(define (whactormap-set! whactormap key val)
  (hash-set! (whactormap-wht whactormap)
             key (make-ephemeron key val)))

(define (snapshot-whactormap whactormap)
  (for/fold ([new-hasheq #hasheq()])
            ([(key eph-val) (whactormap-wht whactormap)])
    (hash-set new-hasheq key eph-val)))

(define (hasheq->whactormap ht #:vat-connector [vat-connector #f])
  (define wht
    (make-weak-hasheq))
  (for ([(key eph-val) ht])
    (hash-set! wht key eph-val))
  (whactormap wht vat-connector))

;;; Now the transactional stuff

(struct transactormap (parent
                       delta
                       [merged? #:mutable]
                       vat-connector)  ; same as parent's, lookup optimization
  #:constructor-name _make-transactormap
  #:methods gen:actormap
  [(define (actormap-ref transactormap key [dflt #f])
     (transactormap-ref transactormap key dflt))
   (define (actormap-set! transactormap key val)
     (transactormap-set! transactormap key val))
   (define (actormap-vat-connector transactormap)
     (transactormap-vat-connector transactormap))])

#;(define actormap?
  (or/c transactormap? actormap?))

(define (make-transactormap parent
                            [vat-connector
                             (actormap-vat-connector parent)])
  (_make-transactormap parent (make-hasheq) #f
                       vat-connector))

(define (transactormap-ref transactormap key [dflt #f])
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-ref on merged transactormap"))
  (define tm-delta
    (transactormap-delta transactormap))
  (if (hash-has-key? tm-delta key)
      ;; we got it, it's in our delta
      (hash-ref tm-delta key)
      ;; search parents for key
      (let ([parent (transactormap-parent transactormap)])
        (match parent
          [(? transactormap?)
           (transactormap-ref parent key dflt)]
          [(? whactormap?)
           (whactormap-ref parent key dflt)]))))

(define (transactormap-set! transactormap key val)
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (hash-set! (transactormap-delta transactormap)
             key val)
  (void))

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
  ;; Serves two functions:
  ;;  - to extract the root weak-hasheq
  ;;  - to merge this transaction on top of the weak-hasheq
  (define (do-merge! transactormap)
    (define parent
      (transactormap-parent transactormap))
    (define root-actormap
      (match parent
        [(? transactormap?)
         (do-merge! parent)]
        [(? whactormap?)
         parent]))
    (unless (transactormap-merged? transactormap)
      (for ([(key val) (transactormap-delta transactormap)])
        (whactormap-set! root-actormap key val))
      (set-transactormap-merged?! transactormap #t))
    root-actormap)
  (do-merge! transactormap)
  (void))

(module+ test
  (require rackunit)

  ;; set up actormap base with beeper and booper
  (define actormap-base (make-whactormap))
  (define beeper-refr (make-local-object-refr 'beeper))
  (define (beeper-proc . args)
    'beep)
  (whactormap-set! actormap-base beeper-refr beeper-proc)
  (define booper-refr (make-local-object-refr 'booper))
  (define (booper-proc . args)
    'boop)
  (whactormap-set! actormap-base booper-refr booper-proc)
  (define blepper-refr (make-local-object-refr 'blepper))
  (define (blepper-proc . args)
    'blep)
  (whactormap-set! actormap-base blepper-refr blepper-proc)

  (define tam1
    (make-transactormap actormap-base))
  (define bipper-refr (make-local-object-refr 'bipper))
  (define (bipper-proc . args)
    'bippity)
  (transactormap-set! tam1 bipper-refr bipper-proc)
  (define (booper-proc2 . args)
    'boop2)
  (transactormap-set! tam1 booper-refr booper-proc2)
  (define (blepper-proc2 . args)
    'blep2)
  (transactormap-set! tam1 blepper-refr blepper-proc2)
  (check-eq? (transactormap-ref tam1 bipper-refr)
             bipper-proc)
  (check-eq? (transactormap-ref tam1 beeper-refr)
             beeper-proc)
  (check-eq? (transactormap-ref tam1 booper-refr)
             booper-proc2)
  (check-eq? (transactormap-ref tam1 blepper-refr)
             blepper-proc2)
  (check-eq? (whactormap-ref actormap-base booper-refr #f)
             booper-proc)
  (check-false (transactormap-merged? tam1))

  (define tam2
    (make-transactormap tam1))

  (define boppiter-refr (make-local-object-refr 'boppiter))
  (define (boppiter-proc . args)
    'boppitty)
  (transactormap-set! tam2 boppiter-refr boppiter-proc)
  (define (booper-proc3 . args)
    'boop3)
  (transactormap-set! tam2 booper-refr booper-proc3)

  (check-eq? (transactormap-ref tam2 beeper-refr)
             beeper-proc)
  (check-eq? (transactormap-ref tam2 booper-refr)
             booper-proc3)
  (check-eq? (transactormap-ref tam2 bipper-refr)
             bipper-proc)
  (check-eq? (transactormap-ref tam2 boppiter-refr)
             boppiter-proc)
  (check-eq? (transactormap-ref tam2 blepper-refr)
             blepper-proc2)
  (check-eq? (whactormap-ref actormap-base booper-refr #f)
             booper-proc)
  (check-eq? (whactormap-ref actormap-base boppiter-refr #f)
             #f)
  (check-false (transactormap-merged? tam2))

  (transactormap-merge! tam2)
  (check-true (transactormap-merged? tam2))
  (check-true (transactormap-merged? tam1))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam2 beeper-refr)))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam1 beeper-refr)))
  (check-exn any/c
             (lambda ()
               (transactormap-set! tam1 beeper-refr
                                   (lambda _ 'whatever))))

  (check-eq? (whactormap-ref actormap-base beeper-refr)
             beeper-proc)
  (check-eq? (whactormap-ref actormap-base booper-refr)
             booper-proc3)
  (check-eq? (whactormap-ref actormap-base bipper-refr)
             bipper-proc)
  (check-eq? (whactormap-ref actormap-base boppiter-refr)
             boppiter-proc)
  (check-eq? (whactormap-ref actormap-base blepper-refr)
             blepper-proc2))

;;; Syscaller internals
;;; ===================

;; NEVER export this.  That would break our security paradigm.
(define current-syscaller
  (make-parameter #f))

(define (get-syscaller-or-die)
  (define sys (current-syscaller))
  (unless sys
    (error "No current syscaller"))
  sys)

(define (call-with-fresh-syscaller actormap proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller actormap))
  (begin0 (parameterize ([current-syscaller sys])
            (proc sys get-sys-internals))
    (close-up!)))

;; In case you want to spawn PROC right off of your vat without
;; involving the syscaller at all
(define (syscaller-free-thread proc)
  (parameterize ([current-syscaller #f])
    (thread proc)))


(define (fresh-syscaller actormap)
  (define vat-connector
    (actormap-vat-connector actormap))
  (define to-near '())
  (define to-far '())

  (define closed? #f)

  (define this-syscaller
    (make-keyword-procedure
     (lambda (kws kw-vals method-id . args)
       (when closed?
         (error "Sorry, this syscaller is closed for business!"))
       (define method
         (case method-id
           ['call _call]
           ['spawn _spawn]
           ['spawn-mactor spawn-mactor]
           ['fulfill-promise fulfill-promise]
           ['break-promise break-promise]
           ;; TODO: These are all variants of 'send-message.
           ;;   Shouldn't we collapse them?
           ['<-np _<-np]
           ['<- _<-]
           ['send-message _send-message]
           ['handle-message _handle-message]
           ['on _on]
           ['listen _listen]
           ['vat-connector get-vat-connector]
           ['near-refr? near-refr?]
           [else (error "invalid syscaller method")]))
       (keyword-apply method kws kw-vals args))))

  (define (near-refr? obj)
    (and (local-refr? obj)
         (eq? (local-refr-vat-connector obj)
              vat-connector)))

  (define (get-vat-connector)
    vat-connector)

  (define (actormap-ref-or-die to-refr)
    (define mactor
      (actormap-ref actormap to-refr #f))
    (unless mactor
      (error 'no-such-actor "no actor with this id in this vat: ~a" to-refr))
    mactor)

  ;; call actor's handler
  (define _call
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       ;; Restrict to live-refrs which appear to have the same
       ;; vat-connector as us
       (unless (local-refr? to-refr)
         (error 'not-callable
                "Not a live reference: ~a" to-refr))

       (unless (eq? (local-refr-vat-connector to-refr)
                    vat-connector)
         (error 'not-callable
                "Not in the same vat: ~a" to-refr))

       (define mactor
         (actormap-ref actormap to-refr))

       (match mactor
         [(? mactor:object?)
          (define actor-handler
            (mactor:object-handler mactor))
          (define become?
            (mactor:object-become? mactor))
          (define become-unsealer
            (mactor:object-become-unsealer mactor))

          ;; I guess watching for this guarantees that an immediate call
          ;; against a local actor will not be tail recursive.
          ;; TODO: We need to document that.
          (define-values (new-handler return-val)
            (let ([returned
                   (call-with-continuation-barrier
                    (λ ()
                      (keyword-apply actor-handler kws kw-vals
                                     args)))])
              (if (become? returned)
                  ;; The unsealer unseals both the handler and return-value anyway
                  (become-unsealer returned)
                  ;; In this case, we're not becoming anything, so just give us
                  ;; the return-val
                  (values #f returned))))

          ;; if a new handler for this actor was specified,
          ;; let's replace it
          (when new-handler
            (actormap-set! actormap to-refr
                           (mactor:object
                            new-handler
                            (mactor:object-become-unsealer mactor)
                            (mactor:object-become? mactor))))

          return-val]
         ;; If it's an encased value, "calling" it just returns the
         ;; internal value.
         [(? mactor:encased?)
          (mactor:encased-val mactor)]
         ;; Ah... we're linking to another actor locally, so let's
         ;; just de-symlink and call that instead.
         [(? mactor:local-link?)
          (keyword-apply _call kws kw-vals
                         (mactor:local-link-point-to mactor)
                         args)]
         ;; Not a callable mactor!
         [_ (error 'not-callable
                   "Not an encased or object mactor: ~a" mactor)]))))

  ;; spawn a new actor
  (define (_spawn constructor kws kw-vals args)
    (actormap-spawn!* actormap constructor kws kw-vals args))

  (define (spawn-mactor mactor [debug-name #f])
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (fulfill-promise promise-id sealed-val)
    (call/ec
     (lambda (return-early)
       (define orig-mactor
         (actormap-ref-or-die promise-id))
       (unless (mactor:unresolved? orig-mactor)
         (error 'resolving-resolved
                "Attempt to resolve resolved actor: ~a" promise-id))
       (define resolve-to-val
         (unseal-mactor-resolution orig-mactor sealed-val))

       (define listeners
         (mactor:unresolved-listeners orig-mactor))

       (define orig-waiting-messages
         (match orig-mactor
           [(? mactor:naive?)
            (mactor:naive-waiting-messages orig-mactor)]
           [(? mactor:closer?)
            (mactor:closer-waiting-messages orig-mactor)]
           [_ '()]))

       (define (forward-messages [waiting-messages orig-waiting-messages])
         (match waiting-messages
           ['() (void)]
           [(list (message _old-to resolve-me kws kw-vals args)
                  rest-waiting ...)
            ;; preserve FIFO by recursing first
            (forward-messages rest-waiting)
            ;; shouldn't be a question message so we don't need to
            ;; #:answer-this-question, I think?
            (_send-message kws kw-vals resolve-to-val resolve-me args)]))

       (define new-waiting-messages
         (if (remote-promise-refr? resolve-to-val)
             ;; don't forward waiting messages to remote promises
             orig-waiting-messages
             ;; but do forward to literally anything else... empty
             ;; the queue!
             (begin (forward-messages)
                    '())))

       (define next-mactor-state
         (match resolve-to-val
           [(? local-object-refr?)
            (when (eq? resolve-to-val promise-id)
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))
            (mactor:local-link resolve-to-val)]
           [(? remote-object-refr?)
            ;; Since the captp connection is the one that might break this,
            ;; we need to ask it what it uses as its resolver unsealer/tm
            ;; @@: ... This doesn't seem like a good solution.
            ;;   Whatever, we need to add when-broken or something.
            (match-define (cons new-resolver-unsealer new-resolver-tm?)
              (let ([connector (remote-refr-captp-connector resolve-to-val)])
                ;; TODO: Do we need to notify it that we want to know about
                ;;   breakage?  Presumably... so do it here instead...?
                (connector 'partition-unsealer-tm-cons)))

            (mactor:remote-link new-resolver-unsealer new-resolver-tm?
                                resolve-to-val)]
           [(or (? local-promise-refr?)
                (? remote-promise-refr?))
            (define new-history
              (if (mactor:closer? orig-mactor)
                  (set-add (mactor:closer-history orig-mactor)
                           (mactor:closer-point-to orig-mactor))
                  (seteq promise-id)))
            ;; Detect cycles!
            (when (set-member? new-history resolve-to-val)
              ;; not sure we actually need to return anything, but I guess
              ;; this is mildly future-proof.
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))

            ;; Make a new set of resolver sealers for this.
            ;; However, we don't use the general ^resolver because we're
            ;; explicitly using the on-fulfilled/on-broken things
            (define-values (new-resolver-sealer new-resolver-unsealer new-resolver-tm?)
              (make-sealer-triplet 'fulfill-promise))
            (define new-resolver
              (_spawn ^resolver '() '() (list promise-id new-resolver-sealer)))
            ;; Now subscribe to the promise...
            (_listen resolve-to-val new-resolver)
            ;; Now we become "closer" to this promise
            (mactor:closer new-resolver-unsealer new-resolver-tm?
                           listeners
                           resolve-to-val new-history
                           new-waiting-messages)]
           ;; anything else is an encased value
           [_ (mactor:encased resolve-to-val)]))

       ;;  - Now actually switch to the new mactor state
       (actormap-set! actormap promise-id
                      next-mactor-state)

       ;; Resolve listeners, if appropriate
       (unless (mactor:unresolved? next-mactor-state)
         (for ([listener listeners])
           (<-np listener 'fulfill resolve-to-val))))))

  ;; TODO: Add support for broken-because-of-network-partition support
  ;;   even for mactor:remote-link
  (define (break-promise promise-id sealed-problem)
    (match (actormap-ref actormap promise-id #f)
      ;; TODO: Not just local-promise, anything that can
      ;;   break
      [(? mactor:unresolved? unresolved-mactor)
       (define problem
         (unseal-mactor-resolution unresolved-mactor sealed-problem))
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                      (mactor:broken problem))
       ;; Inform all listeners of the resolution
       (for ([listener (in-list (mactor:unresolved-listeners unresolved-mactor))])
         (<-np listener 'break problem))]
      [(? mactor:remote-link?)
       (error "TODO: Implement breaking on captp disconnect!")]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve eventual references")]))


  ;; This is the bulk of what's called and handled by actormap-turn-message.
  ;; (As opposed to actormap-turn*, which only supports calling, this also
  ;; handles any toplevel invocation of an actor, probably via message send.)
  (define (_handle-message msg display-or-log-error)
    (match-define (message to-refr resolve-me kws kw-vals args)
      msg)
    (unless (near-refr? to-refr)
      (error 'not-a-near-refr "Not a near refr: ~a" to-refr))

    (define orig-mactor
      (actormap-ref-or-die to-refr))

    ;; Prevent someone trying to throw this vat into an infinite loop
    (when (eq? to-refr resolve-me)
      (error 'same-recipient-and-resolver
             "Recipient and resolver are the same: ~a" to-refr))

    (define (call-with-resolution proc)
      (with-handlers ([exn:fail?
                       (lambda (err)
                         (when display-or-log-error
                           (display-or-log-error err #:pre-delivery? #f))
                         (when resolve-me
                           (_<-np resolve-me 'break err))
                         `#(fail ,err))])
        (define call-result
          (proc))
        (when resolve-me
          (_<-np resolve-me 'fulfill call-result))
        `#(success ,call-result)))

    (match orig-mactor
      ;; If it's callable, we just use the call handler, because
      ;; that's effectively the same code we'd be running anyway.
      ;; However, we do want to handle the resolution.
      [(or (? mactor:object?)
           (? mactor:encased?))
       (call-with-resolution
        (λ () (keyword-apply _call kws kw-vals to-refr args)))]
      [(mactor:local-link point-to)
       (call-with-resolution
        (λ () (keyword-apply _call kws kw-vals point-to args)))]
      [(mactor:broken problem)
       (_<-np resolve-me 'break problem)
       `#(fail ,problem)]
      [(? mactor:remote-link?)
       (define point-to (mactor:remote-link-point-to orig-mactor))
       (call-with-resolution
        (λ ()
          ;; Pass along the message
          ;; Mild optimization: only produce a promise if we have a resolver
          (keyword-apply (if resolve-me
                             _<-
                             _<-np)
                         kws kw-vals point-to args)))]
      ;; Messages sent to a promise that is "closer" are a kind of
      ;; intermediate state; we build a queue.
      [(mactor:closer resolver-unsealer resolver-tm?
                      listeners
                      point-to history
                      waiting-messages)
       (match point-to
         ;; If we're pointing at another near promise then we recurse
         ;; to _handle-messages with the next promise...
         [(? local-promise-refr?)
          ;; (We don't use call-with-resolution because the next one will!)
          (_handle-message (message point-to resolve-me kws kw-vals args)
                           display-or-log-error)]
         ;; But if it's a remote promise then we queue it in the waiting
         ;; messages because we prefer to have messages "swim as close
         ;; as possible to the machine barrier where possible", with
         ;; the exception of questions/answers which always cross over
         ;; (see mactor:question handling later in this procedure)
         [(? remote-promise-refr?)
          ;; Since we're queueing to send the message until it resolves
          ;; we don't resolve the problem here... hence we don't
          ;; use call-with-resolution here either.
          (actormap-set! actormap to-refr
                         (mactor:closer resolver-unsealer resolver-tm?
                                        listeners
                                        point-to history
                                        (cons msg waiting-messages)))
          ;; But we should return that this was deferred
          '#(deferred #f)])]
      ;; Similar to the above w/ remote promises, except that we really
      ;; just don't know where things go *at all* yet, so no swimming
      ;; occurs.
      [(mactor:naive resolver-unsealer resolver-tm?
                     listeners waiting-messages)
       (actormap-set! actormap to-refr
                      (mactor:naive resolver-unsealer resolver-tm?
                                    listeners
                                    (cons msg waiting-messages)))
       '#(deferred #f)]
      ;; Questions should forward their messages to the captp thread
      ;; to deal with using the relevant question-finder.
      ;; In a sense, this is a followup question to an existing
      ;; question.
      [(? mactor:question?)
       (call-with-resolution
        (λ ()
          (define to-question-finder
            (mactor:question-question-finder orig-mactor))
          (define captp-connector
            (mactor:question-captp-connector orig-mactor))
          (define followup-question-finder
            (captp-connector 'new-question-finder))
          (define-values (followup-question-promise followup-question-resolver)
            (_spawn-promise-values #:question-finder
                                   followup-question-finder
                                   #:captp-connector
                                   captp-connector))
          (captp-connector
           'handle-message
           (question-message to-question-finder followup-question-resolver
                             kws kw-vals args
                             followup-question-finder))
          followup-question-promise))]))

  ;; helper to the below two methods
  (define (_send-message kws kw-vals to-refr resolve-me args
                         #:answer-this-question [answer-this-question #f])
    (define new-message
      (if answer-this-question
          (question-message to-refr resolve-me kws kw-vals args
                            answer-this-question)
          (message to-refr resolve-me kws kw-vals args)))

    ;; TODO: This is really a matter of dispatching on mactors
    ;;   mostly now
    (match to-refr
      [(? local-refr?)
       (define in-same-vat?
         (eq? (local-refr-vat-connector to-refr)
              vat-connector))
       (if in-same-vat?
           (set! to-near (cons new-message to-near))
           (set! to-far (cons new-message to-far)))]
      
      [(? remote-refr?)
       (set! to-far (cons new-message to-far))]
      [_ (error 'vat-send-message
                "Don't know how to send a message to: ~a" to-refr)]))

  (define _<-np
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       (_send-message kws kw-vals to-refr #f args)
       (void))))

  (define _<-
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       (match to-refr
         [(? local-refr?)
          (define-values (promise resolver)
            (_spawn-promise-values))
          (_send-message kws kw-vals to-refr resolver args)
          promise]
         [(? remote-refr?)
          (define captp-connector
            (remote-refr-captp-connector to-refr))
          (define question-finder
            (captp-connector 'new-question-finder))
          (define-values (promise resolver)
            (_spawn-promise-values #:question-finder
                                   question-finder
                                   #:captp-connector
                                   captp-connector))
          (_send-message kws kw-vals to-refr resolver args
                         #:answer-this-question question-finder)
          promise]))))

  (define (_listen on-refr listener)
    (match on-refr
      [(? near-refr?)
       (define mactor
         (actormap-ref-or-die on-refr))
       (match mactor
         [(? mactor:local-link?)
          (_listen (mactor:local-link-point-to mactor)
                   listener)]
         ;; This object is a local promise, so we should handle it.
         [(? mactor:eventual?)
          ;; Set a new version of the local-promise with this
          ;; object as a listener
          (actormap-set! actormap on-refr
                         (mactor:unresolved-add-listener mactor
                                                         listener))]
         ;; In the following cases we can resolve the listener immediately...
         [(? mactor:broken? mactor)
          (_<-np listener 'break (mactor:broken-problem mactor))]
         [(? mactor:encased? mactor)
          (_<-np listener 'fulfill (mactor:encased-val mactor))]
         [(? mactor:object? mactor)
          (_<-np listener 'fulfill on-refr)])]

      ;; At this point it would be a far refr
      [(? local-refr? far-refr)
       (define vat-connector
         (local-refr-vat-connector far-refr))
       (vat-connector 'listen far-refr listener)]

      [(? remote-refr? remote-refr)
       (define captp-connector
         (remote-refr-captp-connector remote-refr))
       (captp-connector 'listen remote-refr listener)]
      [val (<-np listener 'fulfill val)]))

  ;; At THIS stage, on-fulfilled, on-broken, on-regardless should
  ;; be actors or #f.  That's not the case in the user-facing
  ;; `on' procedure.
  (define (_on on-refr [on-fulfilled #f]
               #:catch [on-broken #f]
               #:regardless [on-regardless #f]
               #:promise? [promise? #f])
    (define-values (return-promise return-p-resolver)
      (if promise?
          (spawn-promise-values)
          (values #f #f)))

    ;; These two procedures are called once the fulfillment
    ;; or break of the on-refr has actually occurred.
    (define ((handle-resolution on-resolution
                                resolve-fulfill-command) val)
      (cond [on-resolution
             ;; We can't use _send-message directly, because this may
             ;; be in a separate syscaller at the time it's resolved.
             (define syscaller (get-syscaller-or-die))
             ;; But anyway, we want to resolve the return-p-resolver with
             ;; whatever the on-resolution is, which is why we do this goofier
             ;; roundabout
             (syscaller 'send-message
                        '() '() on-resolution
                        ;; Which may be #f!
                        return-p-resolver
                        (list val))
             (when on-regardless
               (<-np on-regardless))]
            ;; There's no on-resolution, which means we can just fulfill
            ;; the promise immediately!
            [else
             (when on-regardless
               (<-np on-regardless))
             (when return-p-resolver
               (<-np return-p-resolver resolve-fulfill-command val))]))
    (define handle-fulfilled
      (handle-resolution on-fulfilled 'fulfill))
    (define handle-broken
      (handle-resolution on-broken 'break))

    ;; The purpose of this listener is that the promise
    ;; *hasn't resolved yet*.  Because of that we need to
    ;; queue something to happen *once* it resolves.
    (define (^on-listener bcom)
      (match-lambda*
        [(list 'fulfill val)
         (handle-fulfilled val)
         (void)]
        [(list 'break problem)
         (handle-broken problem)
         (void)]))
    (define listener
      (_spawn ^on-listener '() '() '()))
    (_listen on-refr listener)
    (when promise?
      return-promise))

  (define (get-internals)
    (list actormap to-near to-far))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))


;;; syscall external functions
;;; ==========================

(define <-np
  (make-keyword-procedure
   (lambda (kws kw-vals to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-vals '<-np to-refr args))))

(define <-
  (make-keyword-procedure
   (lambda (kws kw-vals to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-vals '<- to-refr args))))

(define call
  (make-keyword-procedure
   (lambda (kws kw-vals to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-vals 'call to-refr args))))
;; an alias
(define $ call)

(define spawn
  (make-keyword-procedure
   (lambda (kws kw-vals constructor . args)
     (define sys (get-syscaller-or-die))
     (sys 'spawn constructor kws kw-vals args))))

(define-syntax (define-spawned stx)
  (syntax-parse stx
    [(_ id:id constructor args ...)
     #'(define id
         (spawn (procedure-rename constructor 'id) args ...))]))

(define (on vow [on-fulfilled #f]
            #:catch [on-broken #f]
            #:regardless [on-regardless #f]
            #:promise? [promise? #f])
  (define sys (get-syscaller-or-die))
  (define (maybe-actorize obj proc-name)
    (match obj
      ;; if it's a reference, it's already fine
      [(? live-refr?)
       obj]
      ;; if it's a procedure, let's spawn it
      [(? procedure?)
       (define already-ran
         (make-keyword-procedure
          (lambda _
            (error "Already ran for automatically generated listener"))))
       (spawn
        (procedure-rename
         (lambda (bcom)
           (lambda args
             (bcom already-ran (apply obj args))))
         proc-name))]
      ;; If it's #f, leave it as #f
      [#f #f]
      ;; Otherwise, this doesn't belong here
      [_ (error 'invalid-on-handler
                "Invalid handler for on: ~a" obj)]))
  (sys 'on vow (maybe-actorize on-fulfilled 'on-fulfilled)
       #:catch (maybe-actorize on-broken 'on-broken)
       #:regardless (maybe-actorize on-regardless 'on-regardless)
       #:promise? promise?))

;; Listen to a promise, where listener
(define (listen to-refr listener)
  (define sys (get-syscaller-or-die))
  (sys 'listen to-refr listener))


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr kws kw-vals args
                        #:reckless? [reckless? #f])
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (keyword-apply sys kws kw-vals 'call to-refr args))
     (apply values result-val
            (get-sys-internals)))))  ; actormap to-near to-far

(define actormap-turn
  (make-keyword-procedure
   (lambda (kws kw-vals actormap to-refr . args)
     (define new-actormap
       (make-transactormap actormap))
     (actormap-turn* new-actormap to-refr kws kw-vals args))))

;; Note that this does nothing with the messages.
(define actormap-poke!
  (make-keyword-procedure
   (lambda (kws kw-vals actormap to-refr . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* (make-transactormap actormap)
                       to-refr kws kw-vals args))
     (transactormap-merge! transactormap)
     returned-val)))

(define actormap-reckless-poke!
  (make-keyword-procedure
   (lambda (kws kw-vals actormap to-refr . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-refr kws kw-vals args))
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-peek
  (make-keyword-procedure
   (lambda (kws kw-vals actormap to-refr . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* (make-transactormap actormap)
                       to-refr kws kw-vals args))
     returned-val)))

(define (simple-display-error err
                              #:pre-delivery?
                              [pre-delivery? #f])
  (displayln (if pre-delivery?
                 ";; === Before even being able to handle message: ==="
                 ";; === While attempting to handle message: ===")
             (current-error-port))
  ((error-display-handler) (exn-message err) err))

(define no-op
  (make-keyword-procedure
   (lambda _ (void))))

;; TODO: We might want to return one of the following:
;;   (values ('call-success val) ('resolve-success val)
;;           actormap to-near to-far)
;;   (values ('call-fail problem) ('resolve-fail problem)
;;           actormap to-near to-far)
;;   (values ('call-success val) #f  ; there was nothing to resolve
;;           actormap to-near to-far)
;; Mix and match the fail/success
(define (actormap-turn-message actormap msg
                               #:display-or-log-error
                               [display-or-log-error simple-display-error])
  ;; TODO: Kuldgily reimplements part of actormap-turn*... maybe
  ;; there's some opportunity to combine things, dunno.
  (call-with-fresh-syscaller
   (make-transactormap actormap)
   (lambda (sys get-sys-internals)
     (define call-result
       (with-handlers ([exn:fail?
                        (lambda (err)
                          ;; TODO: Maybe make clear that this is even more
                          ;;   fundamental error?  Note that the resolver might
                          ;;   not even be resolved.  Goofy approach to that
                          ;;   for now...
                          (when display-or-log-error
                            (display-or-log-error err #:pre-delivery? #t))
                          `#(fail ,err))])
         (sys 'handle-message msg display-or-log-error)))

     (match (get-sys-internals)
       [(list new-actormap to-near to-far)
        (values call-result new-actormap to-near to-far)]))))

;; The following two are utilities for when you want to check
;; or bootstrap something within an actormap

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val _am _tl _tr)
    (actormap-run* (make-transactormap actormap) thunk))
  returned-val)

;; like actormap-run but also returns the new actormap, to-near, to-far
(define (actormap-run* actormap thunk)
  (define-values (actor-refr new-actormap)
    (actormap-spawn (make-transactormap actormap) (lambda (bcom) thunk)))
  (define-values (returned-val new-actormap2 to-near to-far)
    (actormap-turn* (make-transactormap new-actormap) actor-refr '() '() '()))
  (values returned-val new-actormap2 to-near to-far))

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define (actormap-run! actormap thunk
                       #:reckless? [reckless? #f])
  (define actor-refr
    (actormap-spawn! actormap (lambda (bcom) thunk)))
  (define actormap-poker!
    (if reckless?
        actormap-reckless-poke!
        actormap-poke!))
  (actormap-poker! actormap actor-refr))


;; Returns a new tree of local messages
;; TODO: Note this pretty much throws out remote messages, for better or
;; almost certainly for worse.
;; (-> actormap? (treeof message?)
;;     (values actormap (treeof message?) (treeof message?)))
(define (actormap-churn actormap messages
                        #:display-errors?
                        [display-errors? #t]
                        #:display-or-log-error
                        [display-or-log-error
                         (if display-errors?
                             simple-display-error
                             no-op)])
  (define (cons-if-non-empty a d)
    (cond
      [(null? a) d]
      [(null? d) a]
      [else (cons a d)]))
  (match messages
    [(? message? message)
     (define-values (call-result new-am to-near to-far)
       (actormap-turn-message actormap messages
                              #:display-or-log-error display-or-log-error))
     (values new-am to-near to-far)]
    ['()
     (values actormap '() '())]
    [(? pair? message-list)
     (for/fold ([actormap actormap]
                [to-near '()]
                [to-far '()])
               ([msg (in-list (reverse messages))])
       (define-values (new-actormap new-to-near new-to-far)
         (actormap-churn actormap msg
                         #:display-or-log-error display-or-log-error))
       (values new-actormap
               (cons-if-non-empty new-to-near to-near)
               (cons-if-non-empty new-to-far to-far)))]))

;; Start up an actormap and run until no more messages are left.
;; Not really used in combination with hives; this is mainly
;; to make some simpler patterns easier to test.
;; TODO: Can we generalize/parameterize this in such a way that
;; vats and other designs can be built on top of it?
(define (actormap-full-run! actormap thunk
                            #:display-errors?
                            [display-errors? #t]
                            #:display-or-log-error
                            [display-or-log-error
                             (if display-errors?
                                 simple-display-error
                                 no-op)])
  (define-values (_val new-am to-near to-far)
    (actormap-run* actormap thunk))
  (transactormap-merge! new-am)
  (let lp ([messages to-near])
    (define-values (new-am to-near to-far)
      (actormap-churn actormap messages
                      #:display-or-log-error display-or-log-error))
    (when (transactormap? new-am)
      (transactormap-merge! new-am))
    (if (null? to-near)
        (void)
        (lp to-near)))
  _val)


;; Spawning
;; ========

;; This is the internally used version of actormap-spawn,
;; also used by the syscaller.  It doesn't set up a syscaller
;; if there isn't currently one.
(define (actormap-spawn!* actormap actor-constructor
                          kws kw-vals args)
  (define debug-name
    (object-name actor-constructor))
  (define vat-connector
    (actormap-vat-connector actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (define actor-handler
    (keyword-apply actor-constructor kws kw-vals become args))
  (match actor-handler
    ;; New procedure, so let's set it
    [(? procedure?)
     (define actor-refr
       (make-local-object-refr debug-name vat-connector))
     (actormap-set! actormap actor-refr
                    (mactor:object actor-handler
                                         become-unseal become?))
     actor-refr]
    [(? live-refr? pre-existing-refr)
     pre-existing-refr]
    [_
     (error 'invalid-actor-handler "Not a procedure or live refr: ~a" actor-handler)]))

;; These two are user-facing procedures.  Thus, they set up
;; their own syscaller.

;; non-committal version of actormap-spawn
(define actormap-spawn
  (make-keyword-procedure
   (lambda (kws kw-vals actormap actor-constructor . args)
     (define new-actormap
       (make-transactormap actormap))
     (call-with-fresh-syscaller
      new-actormap
      (lambda (sys get-sys-internals)
        (define actor-refr
          (actormap-spawn!* new-actormap actor-constructor
                            kws kw-vals args))
        (values actor-refr new-actormap))))))

(define actormap-spawn!
  (make-keyword-procedure
   (lambda (kws kw-vals actormap actor-constructor . args)
     (define new-actormap
       (make-transactormap actormap))
     (define actor-refr
       (call-with-fresh-syscaller
        new-actormap
        (lambda (sys get-sys-internals)
          (actormap-spawn!* new-actormap actor-constructor
                            kws kw-vals args))))
     (transactormap-merge! new-actormap)
     actor-refr)))

(define (actormap-spawn-mactor! actormap mactor
                                [debug-name #f])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (if (mactor:object? mactor)
        (make-local-object-refr debug-name vat-connector)
        (make-local-promise-refr vat-connector)))
  (actormap-set! actormap actor-refr mactor)
  actor-refr)

(module+ test
  (require rackunit
           racket/contract)
  (define am (make-whactormap))

  ;; Copy of the cell code from cell.rkt.  Simplifies some
  ;; tests.

  ;; Constructor for a cell.  Takes an optional initial value, defaults
  ;; to false.
  (define (^cell bcom [val #f])
    (case-lambda
      ;; Called with no arguments; return the current value
      [() val]
      ;; Called with one argument, we become a version of ourselves
      ;; with this new value
      [(new-val)
       (bcom (^cell bcom new-val))]))

  (define ((counter bcom n))
    (bcom (counter bcom (add1 n))
          n))

  ;; can actors update themselves?
  (define ctr-refr
    (actormap-spawn! am counter 1))
  (define-values (turned-val1 am+ctr1 _to-near _to-far)
    (actormap-turn am ctr-refr))
  (check-eqv? turned-val1 1)
  (define-values (turned-val2 am+ctr2 _to-near2 _to-far2)
    (actormap-turn am+ctr1 ctr-refr))
  (check-eqv? turned-val2 2)

  ;; transaction shouldn't be applied yet
  (define-values (turned-val1-again
                  am+ctr1-again
                  _to-near-again _to-far-again)
    (actormap-turn am ctr-refr))
  (check-eqv? turned-val1-again 1)

  ;; but now it should be
  (transactormap-merge! am+ctr2)
  (define-values (turned-val3 am+ctr3 _to-near3 _to-far3)
    ;; observe that we're turning using the "original"
    ;; actormap though!  It should have committed.
    (actormap-turn am ctr-refr))
  (check-eqv? turned-val3 3)

  (define ((^friend-spawner bcom) friend-name)
    (define (^a-friend bcom)
      (define ((next called-times))
        (bcom (next (add1 called-times))
              (format "Hello!  My name is ~a and I've been called ~a times!"
                      friend-name called-times)))
      (next 1))
    (spawn ^a-friend))
  (define fr-spwn (actormap-spawn! am ^friend-spawner))
  (define joe (actormap-poke! am fr-spwn 'joe))
  (check-equal?
   (actormap-peek am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-poke! am joe)
   "Hello!  My name is joe and I've been called 1 times!")
  (check-equal?
   (actormap-poke! am joe)
   "Hello!  My name is joe and I've been called 2 times!")

  (define-values (noncommital-refr noncommital-am)
    (actormap-spawn am (lambda (bcom)
                         (lambda _ 'noncommital))))
  (check-eq?
   (actormap-peek noncommital-am noncommital-refr)
   'noncommital)
  (check-exn
   any/c
   (lambda () (actormap-peek am noncommital-refr)))

  (define who-ya-gonna-call
    #f)
  (define (make-and-call-friend)
    (define new-friend-spawner
      (spawn ^friend-spawner))
    (define friend
      (call new-friend-spawner 'brian))
    (set! who-ya-gonna-call friend)
    (call friend))
  (check-equal?
   (actormap-run am make-and-call-friend)
   "Hello!  My name is brian and I've been called 1 times!")
  (check-exn
   any/c
   (lambda () (actormap-peek am who-ya-gonna-call)))
  ;; now run it with commitment
  (check-equal?
   (actormap-run! am make-and-call-friend)
   "Hello!  My name is brian and I've been called 1 times!")
  (check-equal?
   (actormap-peek noncommital-am who-ya-gonna-call)
   "Hello!  My name is brian and I've been called 2 times!")

  ;; Now let's make sure that during actormap-spawn(!) that we can
  ;; spawn other things

  (define (^spawns-during-constructor bcom)
    (define a-cell
      (spawn ^cell 'foo))
    (lambda ()
      (list 'got ($ a-cell))))
  (define sdc
    (actormap-spawn! am ^spawns-during-constructor))

  (test-equal?
   "Spawn when we actormap-spawn(!) (yo dawg)"
   (actormap-peek am sdc)
   '(got foo))

  (let ([inner-refr #f])
    (test-eq?
     "Spawn can instead just return another actor refr instead of a new object"
     (actormap-spawn! am
                      (lambda (bcom)
                        (define ir
                          (spawn (lambda (bcom)
                                   (lambda ()
                                     'hi-friend))))
                        (set! inner-refr ir)
                        ir))
     inner-refr)))

;;; Some simple helpers
;;; ===================

;; Spawns a procedure that will never become anything else
(define (spawn-proc proc)
  (spawn (lambda _ proc)))

(define (spawn-const val)
  (spawn (lambda _ (lambda _ val))))


;;; Promises
;;; ========

;; I guess the alternatives to responding with false on
;; attempting to re-resolve are:
;;  - throw an error
;;  - just return void regardless
(define already-resolved
  (lambda _ #f))

(define (^resolver bcom promise sealer)
  (match-lambda*
    [(list 'fulfill val)
     (define sys (get-syscaller-or-die))
     (sys 'fulfill-promise promise (sealer val))
     (bcom already-resolved)]
    [(list 'break problem)
     (define sys (get-syscaller-or-die))
     (sys 'break-promise promise (sealer problem))
     (bcom already-resolved)]))

(define (_spawn-promise-values #:question-finder
                               [question-finder #f]
                               #:captp-connector
                               [captp-connector #f])
  (define-values (sealer unsealer tm?)
    (make-sealer-triplet 'fulfill-promise))
  (define sys (get-syscaller-or-die))
  (define promise
    (sys 'spawn-mactor
         (if question-finder
             (begin
               (unless captp-connector
                 (error 'question-finder-without-captp-connector))
               (mactor:question unsealer tm? '()
                                captp-connector
                                question-finder))
             (mactor:naive unsealer tm? '() '()))))
  (define resolver
    (spawn ^resolver promise sealer))
  (values promise resolver))

(module+ for-captp
  (provide _spawn-promise-values
           make-remote-object-refr
           make-remote-promise-refr
           remote-refr-captp-connector
           remote-refr-sealed-pos))

(define (spawn-promise-values)
  (_spawn-promise-values))

(define (spawn-promise-cons)
  (call-with-values spawn-promise-values cons))

(module+ test
  (define bob (actormap-spawn! am ^cell "Hi, I'm bob!"))
  (match-define (cons bob-vow bob-resolver)
    (actormap-run! am spawn-promise-cons))
  (check-not-exn
   (lambda ()
     (actormap-poke! am bob-resolver 'fulfill bob)))
  (test-true
   "Promise resolves to local-link"
   (mactor:local-link? (whactormap-ref am bob-vow)))
  (test-equal?
   "Resolved local-link acts as what it resolves to"
   (actormap-peek am bob-vow)
   "Hi, I'm bob!")
  (check-not-exn
   (lambda ()
     (actormap-poke! am bob-vow "Hi, I'm bobby!")))
  (test-equal?
   "Resolved local-link can change original"
   (actormap-peek am bob)
   "Hi, I'm bobby!")

  (define on-resolved-bob-arg #f)
  (actormap-full-run!
   am (lambda ()
        (on bob-vow
            (lambda (v)
              (set! on-resolved-bob-arg v)))))
  (test-equal?
   "Using `on' against a resolved refr returns that refr"
   on-resolved-bob-arg bob)


  (match-define (cons encase-me-vow encase-me-resolver)
    (actormap-run! am spawn-promise-cons))
  (actormap-poke! am encase-me-resolver 'fulfill 'encase-me)
  (test-eq?
   "extracting encased value via actormap-peek"
   (actormap-peek am encase-me-vow)
   'encase-me)
  (test-eq?
   "extracting encased value via $"
   (actormap-run am (lambda () ($ encase-me-vow)))
   'encase-me)

  (define on-resolved-encased-arg #f)
  (actormap-full-run!
   am (lambda ()
        (on encase-me-vow
            (lambda (v)
              (set! on-resolved-encased-arg v)))))
  (test-equal?
   "Using `on' against a resolved refr returns that refr"
   on-resolved-encased-arg 'encase-me)


  ;; Tests for propagation of resolutions
  (define (try-out-on actormap . resolve-args)
    ;; Run on against a promise, store the results
    ;; in the following cells
    (define resolved-cells
      (actormap-full-run!
       actormap
       (lambda ()
         (define fulfilled-cell (spawn ^cell #f))
         (define broken-cell (spawn ^cell #f))
         (define regardless-cell (spawn ^cell #f))
         (define-values (a-vow a-resolver)
           (spawn-promise-values))
         (on a-vow
             (lambda args
               ($ fulfilled-cell args))
             #:catch
             (lambda args
               ($ broken-cell args))
             #:regardless
             (lambda ()
               ($ regardless-cell #t)))
         (apply <-np a-resolver resolve-args)
         (list fulfilled-cell broken-cell regardless-cell))))
    (map (lambda (cell)
           (actormap-peek actormap cell))
         resolved-cells))

  (test-equal?
   "Fulfilling a promise with on"
   (try-out-on am 'fulfill 'how-fulfilling)
   '((how-fulfilling) #f #t))

  (test-equal?
   "Breaking a promise with on"
   (try-out-on am 'break 'i-am-broken)
   '(#f (i-am-broken) #t))

  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (on (<- (spawn-const 'i-am-foo))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e))))))
    (test-equal?
     "<- returns a listen'able promise"
     what-i-got
     '(yeah i-am-foo)))

  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (on (<- (spawn-proc
                    (lambda _
                      (error "I am error"))))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e)))))
     #:display-errors? #f)
    (test-equal?
     "<- promise breaks as expected"
     (car what-i-got)
     'oh-no))
  
  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (define foo (spawn-const 'i-am-foo))
          (on (<- (<- (spawn-proc (lambda _ foo))))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e))))))
    (test-equal?
     "basic promise pipelining"
     what-i-got
     '(yeah i-am-foo)))

  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (define fatal-foo
            (spawn-proc
             (lambda _
               (error "I am error"))))
          (on (<- (<- (spawn-proc (lambda _ fatal-foo))))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e)))))
     #:display-errors? #f)
    (test-equal?
     "basic promise contagion"
     (car what-i-got)
     'oh-no))

  (test-equal?
   "Passing #:promise? to `on` returns a promise that is resolved"
   (actormap-peek
    am
    (actormap-full-run!
     am (lambda ()
          (define doubler (spawn (lambda (bcom)
                                   (lambda (x)
                                     (* x 2)))))
          (define the-on-promise
            (on (<- doubler 3)
                (lambda (x)
                  (format "got: ~a" x))
                #:catch
                (lambda (e)
                  "uhoh")
                #:promise? #t))
          the-on-promise)))
   "got: 6")

  (let ([what-i-got #f]
        [regardless-also-ran? #f])
    (actormap-full-run!
     am
     (lambda ()
       (on 42
           (lambda (val)
             (set! what-i-got (format "got: ~a" val)))
           #:regardless
           (lambda ()
             (set! regardless-also-ran? #t)))))
    (test-equal?
     "A non-promise value passed to `on` merely resolves to that value"
     what-i-got
     "got: 42")
    (test-true
     "#:regardless also runs in case of non-promise value passed to `on`"
     regardless-also-ran?))

  ;; verify listen works
  (define (try-out-listen . resolve-args)
    (let ([resolved-val #f]
          [resolved-err #f])
      (actormap-full-run!
       am
       (lambda ()
         (match-define (cons some-promise some-resolver)
           (actormap-run! am spawn-promise-cons))
         (listen some-promise
                 (spawn
                  (lambda (bcom)
                    (match-lambda*
                      [(list 'fulfill val)
                       (set! resolved-val `(fulfilled ,val))]
                      [(list 'break err)
                       (set! resolved-err `(broken ,err))]))))
         (apply $ some-resolver resolve-args)))
      (list resolved-val resolved-err)))
  (test-equal?
   "listen works with a fulfilled promise"
   (try-out-listen 'fulfill 'yay)
   '((fulfilled yay) #f))
  (test-equal?
   "listen works with a broken promise"
   (try-out-listen 'break 'oh-no)
   '(#f (broken oh-no))))
