#lang racket/base

;;; Exports
;;; =======

(require (for-syntax syntax/parse racket/base)
         racket/contract
         racket/set
         "utils/simple-sealers.rkt")

;; Refrs
;; Same as E refs, but -ref is already meaningful in scheme
(provide refr?
         live-refr?
         sturdy-refr?
         near-refr?
         #;callable?)

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

         on
         <-np <-

         spawn-proc spawn-const

         spawn-promise-values spawn-promise-cons)


(module+ debug
  (provide whactormap-wht))

;; meh...
(module+ for-vats
  (provide live-refr-vat-connector))

;;; Imports
;;; =======

(require "message.rkt"
         "hash-contracts.rkt"
         racket/match
         racket/generic)


;;; Refrs
;;; =====

(struct refr ())

(struct live-refr refr (debug-name vat-connector)
  #:constructor-name _make-live-refr
  #:methods gen:custom-write
  [(define (write-proc refr port mode)
     (define str-to-write
       (match (live-refr-debug-name refr)
         [#f "#<live-refr>"]
         ;; TODO: Do we need to do escaping?
         [debug-name (format "#<live-refr ~a>" debug-name)]))
     (write-string str-to-write port))])

(define (make-live-refr [debug-name #f] [vat-connector #f])
  (_make-live-refr debug-name vat-connector))

(struct sturdy-refr refr (swiss-num vat-id conn-hints))

(struct vat-connid ()
  #:constructor-name make-vat-connid)

;;; Meta-actors and miranda methods
;;; ===============================

(module+ mactor-extra
  (provide mactor mactor?
           mactor:local mactor:local?
           mactor:remote mactor:remote?

           mactor:local-actor mactor:local-actor?
           mactor:local-actor-handler
           mactor:local-actor-become-unsealer
           mactor:local-actor-become?

           mactor:encased mactor:encased?
           mactor:encased-val

           mactor:remote-actor mactor:remote-actor?

           mactor:symlink mactor:symlink?
           mactor:symlink-link-to-refr

           mactor:broken mactor:broken?
           mactor:broken-problem

           mactor:local-promise mactor:local-promise?
           mactor:local-promise-listeners
           mactor:local-promise-resolver-unsealer
           mactor:local-promise-resolver-tm?

           mactor:remote-promise mactor:remote-promise?))

;; We need these to have different behavior, equivalent to E's
;; "miranda rights":
;; http://www.erights.org/elang/blocks/miranda.html
;;
;; However we haven't implemented all that functionality *quite* yet.

;;; Mactors fall into two general categories:
;;;  - eventual
;;;  - resolved

;;; Additionally, location-wise:
;;;  - near?:       same vat
;;;  - far?:        different vat
;;;  - local?:      same machine
;;;  - remote?:     different machine

(struct mactor ())
(struct mactor:local mactor ())
;; TODO: Will this ever exist?  It might only ever be a symlink.
(struct mactor:remote mactor (vat-connid))


;;; Resolved things
;;; ---------------
;; once a local refr, always a local refr.
(struct mactor:local-actor mactor:local
  (handler become-unsealer become?))

;; Once encased, always encased.
;; TODO: Maybe we don't need mactors for this.  Maybe anything that's
;;   not a mactor is basically "encased"?  But having an official
;;   mactor type gives us a clearer answer I guess.
(struct mactor:encased mactor (val))
(struct mactor:remote-actor mactor:remote ())
(struct mactor:symlink mactor (link-to-refr))
;; Once broken, always broken.
(struct mactor:broken mactor (problem))

;;; Eventual things
;;; ---------------
(struct mactor:local-promise mactor:local
  (listeners resolver-unsealer resolver-tm?))
(struct mactor:remote-promise mactor:remote ())

;; TODO: Whatever procedure we make for these we need to
;;   operate with a refr indirection; we won't have the mactor
#;(define local-refr?
  (procedure-rename mactor:local? 'local-refr?))
#;(define far-refr?
  (procedure-rename mactor:remote? 'far-refr?))

;; Determines whether immediately callable within the current
;; syscaller context
;; TODO: Needs tests!  Well, we could do them in vat.rkt...
#;(define (callable? obj)
  (let ([sys (current-syscaller)])
    (and sys
         (sys 'callable? obj))))

;; Presumes this is a mactor already
(define (callable-mactor? mactor)
  (or (mactor:local-actor? mactor)
      (mactor:encased? mactor)))

(define (near-refr? refr)
  ((current-syscaller) 'near-refr? refr))

;;; "Become" special sealers
;;; ========================

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
  (define beeper-refr (make-live-refr 'beeper))
  (define (beeper-proc . args)
    'beep)
  (whactormap-set! actormap-base beeper-refr beeper-proc)
  (define booper-refr (make-live-refr 'booper))
  (define (booper-proc . args)
    'boop)
  (whactormap-set! actormap-base booper-refr booper-proc)
  (define blepper-refr (make-live-refr 'blepper))
  (define (blepper-proc . args)
    'blep)
  (whactormap-set! actormap-base blepper-refr blepper-proc)

  (define tam1
    (make-transactormap actormap-base))
  (define bipper-refr (make-live-refr 'bipper))
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

  (define boppiter-refr (make-live-refr 'boppiter))
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

(define (actormap-symlink-ref actormap refr-id)
  (let lp ([refr-id refr-id]
           [seen (seteq)])
    (when (set-member? seen refr-id)
      (error "Cycle in mactor symlinks"))
    (match (actormap-ref actormap refr-id #f)
      [(? mactor:symlink? mactor)
       (lp (mactor:symlink-link-to-refr mactor)
           (set-add seen refr-id))]
      [#f (error "no actor with this id" refr-id)]
      [mactor (values refr-id mactor)])))

(define (fresh-syscaller actormap)
  (define vat-connector
    (actormap-vat-connector actormap))
  (define to-near '())
  (define to-far '())

  (define closed? #f)

  (define this-syscaller
    (make-keyword-procedure
     (lambda (kws kw-args method-id . args)
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
           ['vat-connector get-vat-connector]
           ['near-refr? near-refr?]
           [else (error "invalid syscaller method")]))
       (keyword-apply method kws kw-args args))))

  (define (near-refr? obj)
    (and (live-refr? obj)
         (eq? (live-refr-vat-connector obj)
              vat-connector)))

  (define (get-vat-connector)
    vat-connector)

  ;; helper procedure, used by a couple of things
  ;; By this point, update-refr should be de-symlink'ed
  (define (__really-do-call update-refr mactor
                            kws kw-args args)
    (call-with-continuation-barrier
     (λ ()
       (match mactor
         [(? mactor:local-actor?)
          (define actor-handler
            (mactor:local-actor-handler mactor))
          (define become?
            (mactor:local-actor-become? mactor))
          (define become-unsealer
            (mactor:local-actor-become-unsealer mactor))

          ;; I guess watching for this guarantees that an immediate call
          ;; against a local actor will not be tail recursive.
          ;; TODO: We need to document that.
          (define-values (new-handler return-val)
            (let ([returned (keyword-apply actor-handler kws kw-args
                                           args)])
              (if (become? returned)
                  ;; The unsealer unseals both the handler and return-value anyway
                  (become-unsealer returned)
                  ;; In this case, we're not becoming anything, so just give us
                  ;; the return-val
                  (values #f returned))))

          ;; if a new handler for this actor was specified,
          ;; let's replace it
          (when new-handler
            (actormap-set! actormap update-refr
                           (mactor:local-actor
                            new-handler
                            (mactor:local-actor-become-unsealer mactor)
                            (mactor:local-actor-become? mactor))))

          return-val]
         ;; If it's an encased value, "calling" it just returns the
         ;; internal value.
         [(? mactor:encased?)
          (mactor:encased-val mactor)]
         [_ (error 'not-callable
                   "Not an encased or live-actor mactor: ~a" mactor)]))))

  ;; call actor's handler
  (define _call
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       ;; Restrict to live-refrs which appear to have the same
       ;; vat-connector as us
       (unless (live-refr? to-refr)
         (error 'not-callable
                "Not a live reference: ~a" to-refr))

       (unless (eq? (live-refr-vat-connector to-refr)
                    vat-connector)
         (error 'not-callable
                "Not in the same vat: ~a" to-refr))

       (define-values (update-refr mactor)
         (actormap-symlink-ref actormap to-refr))

       (__really-do-call update-refr mactor kws kw-args args))))

  ;; spawn a new actor
  (define (_spawn constructor kws kw-args args)
    (actormap-spawn!* actormap constructor kws kw-args args))

  (define (spawn-mactor mactor [debug-name #f])
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (fulfill-promise promise-id sealed-val)
    (match (actormap-ref actormap promise-id #f)
      [(? mactor:local-promise? promise-mactor)
       (define resolver-tm?
         (mactor:local-promise-resolver-tm? promise-mactor))
       (define resolver-unsealer
         (mactor:local-promise-resolver-unsealer promise-mactor))
       ;; Is this a valid resolution?
       (unless (resolver-tm? sealed-val)
         (error "Resolution sealed with wrong trademark!"))
       (define val
         (resolver-unsealer sealed-val))

       ;; Inform all listeners of the resolution
       ;; We'll do this unless we're symlinking to another promise,
       ;; in which case we just "pass on" the listeners.
       (define (inform-listeners)
         (for ([listener (mactor:local-promise-listeners promise-mactor)])
           (<-np listener 'fulfill val)))

       ;; Now we "become" that value!
       (match val
         ;; It's a reference now, so let's set up a symlink
         [(? refr?)
          ;; for efficiency, let's make it as direct of a symlink
          ;; as possible
          (define-values (link-to-refr link-to-mactor)
            (let lp ([refr-id val]
                     [seen (seteq)])
              (when (set-member? seen refr-id)
                (error "Cycle in mactor symlinks"))
              ;; TODO: deal with far refrs
              (if (near-refr? refr-id)
                  (match (actormap-ref actormap refr-id)
                    [(? mactor:symlink? mactor)
                     (lp (mactor:symlink-link-to-refr mactor)
                         (set-add seen refr-id))]
                    [#f (error "no actor with this id")]
                    ;; ok we found a non-symlink refr
                    [mactor (values refr-id mactor)])
                  ;; otherwise it's a far-refr, we can't really
                  ;; de-symlink anymore.
                  (values refr-id #f))))
          ;; Set up the symlink
          (actormap-set! actormap promise-id
                         (mactor:symlink link-to-refr))
          ;; Now we either inform listeners or forward them to the promise
          (match link-to-mactor
            ;; Ok, it's a local promise...
            ;; For this we still need to set the symlink, but
            ;; we should defer our sending of messages until the
            ;; other promise resolves.
            [(mactor:local-promise linked-listeners linked-r-unsealer linked-r-tm?)
             ;; Update the symlinked-to-promise to have all of our listeners
             (define new-linked-listeners
               (append (mactor:local-promise-listeners promise-mactor)
                       linked-listeners))
             (define new-linked-mactor
               (mactor:local-promise new-linked-listeners
                                     linked-r-unsealer
                                     linked-r-tm?))
             (actormap-set! actormap link-to-refr
                            new-linked-mactor)]

            ;; Nope it's not a promise, so inform listeners now
            [_ (inform-listeners)])]
         ;; Must be something else then.  Guess we'd better
         ;; encase it.
         [_ (actormap-set! actormap promise-id
                           (mactor:encased val))
            (inform-listeners)])]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a local-promise")]))

  (define (break-promise promise-id sealed-problem)
    (match (actormap-ref actormap promise-id #f)
      ;; TODO: Not just local-promise, anything that can
      ;;   break
      [(? mactor:local-promise? promise-mactor)
       (define resolver-tm?
         (mactor:local-promise-resolver-tm? promise-mactor))
       (define resolver-unsealer
         (mactor:local-promise-resolver-unsealer promise-mactor))
       ;; Is this a valid resolution?
       (unless (resolver-tm? sealed-problem)
         (error "Resolution sealed with wrong trademark!"))
       (define problem
         (resolver-unsealer sealed-problem))
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                           (mactor:broken problem))
       ;; Inform all listeners of the resolution
       (for ([listener (mactor:local-promise-listeners promise-mactor)])
         (<-np listener 'break problem))]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a local-promise")]))

  (define (_handle-message msg)
    (match-define (message to-refr resolve-me kws kw-vals args)
      msg)
    (unless (near-refr? to-refr)
      (error 'not-a-near-refr "Not a near refr: ~a" to-refr))

    (define-values (update-refr mactor)
      (actormap-symlink-ref actormap to-refr))

    (match mactor
      ;; If it's callable, we just use the call handler, because
      ;; that's effectively the same code we'd be running anyway.
      [(? callable-mactor?)
       (keyword-apply _call kws kw-vals to-refr args)]
      ;; If it's a promise, that means we're queuing up something to
      ;; run *once this promise is resolved*.
      [(? mactor:local-promise?)
       ;; Create new actor that is subscribed to this
       ;; TODO: Really important!  We need to detect a cycle to prevent
       ;;   going in loops on accident.
       ;;   I'm not actually sure how to do that yet...
       ;; TODO: We've got an unncecessary promise-to-a-promise
       ;;   indirection via this method, which we could cut out
       ;;   the middleman of I think?
       (_on update-refr
            (let ([promise-pipeline-helper
                   (lambda (bcom)
                     (lambda (send-to)
                       (keyword-apply <- kws kw-vals send-to args)))])
              (_spawn promise-pipeline-helper '() '() '()))
            ;; Wait, what will this do for us?  Wouldn't it
            ;; just return another void?
            #:promise? #t)]
      ;; If it's broken, re-raise the problem.
      ;; TODO: maybe re-raising isn't the right route?  Though I think
      ;; it does work technically.  It's the easiest solution...
      [(mactor:broken problem)
       (raise problem)]))

  ;; helper to the below two methods
  (define (_send-message kws kw-args to-refr resolve-me args)
    (define new-message
      (message to-refr resolve-me kws kw-args args))

    ;; TODO: This is really a matter of dispatching on mactors
    ;;   mostly now
    (match to-refr
      [(? live-refr?)
       (define in-same-vat?
         (eq? (live-refr-vat-connector to-refr)
              vat-connector))
       (if in-same-vat?
           (set! to-near (cons new-message to-near))
           (set! to-far (cons new-message to-far)))]
      #;[(? far-refr?)
       (set! to-far (cons new-message to-far))]
      [_ (error 'vat-send-message
                "Don't know how to send a message to: ~a" to-refr)]))

  (define _<-np
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       (_send-message kws kw-args to-refr #f args)
       (void))))

  (define _<-
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       (define-values (promise resolver)
         (spawn-promise-values))
       (_send-message kws kw-args to-refr resolver args)
       promise)))

  ;; At THIS stage, on-fulfilled, on-broken, on-regardless should
  ;; be actors or #f.  That's not the case in the user-facing
  ;; `on' procedure.
  (define (_on id-refr [on-fulfilled #f]
               #:catch [on-broken #f]
               #:regardless [on-regardless #f]
               #:promise? [promise? #f])
    (define-values (return-promise return-p-resolver)
      (if promise?
          (spawn-promise-values)
          (values #f #f)))

    ;; These two procedures are called once the fulfillment
    ;; or break of the id-refr has actually occurred.
    (define ((handle-resolution on-resolution
                                resolve-fulfill-command) val)
      (cond [on-resolution
             ;; We can't use _send-message directly, because this may
             ;; be in a separate syscaller at the time it's resolved.
             (define syscaller (get-syscaller-or-die))
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

    (match id-refr
      [(? live-refr?)
       (define-values (subscribe-refr mactor)
         (actormap-symlink-ref actormap id-refr))

       (match mactor
         ;; This object is a local promise, so we should handle it.
         [(mactor:local-promise listeners r-unsealer r-tm?)
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
          (define on-listener
            ;; using _spawn here saves a very minor round
            ;; trip which we can't do in the on-fulfilled
            ;; ones because they'll be in a new syscaller
            (_spawn ^on-listener '() '() '()))
          ;; Set a new version of the local-promise with this
          ;; object as
          (define new-listeners
            (cons on-listener listeners))
          (actormap-set! actormap id-refr
                         (mactor:local-promise new-listeners
                                               r-unsealer r-tm?))]
         [(? mactor:broken? mactor)
          (handle-broken (mactor:broken-problem mactor))]
         [(? mactor:encased? mactor)
          (handle-fulfilled (mactor:encased-val mactor))]
         [(? (or/c mactor:remote? mactor:local-actor?) mactor)
          (handle-fulfilled subscribe-refr)]
         ;; This involves invoking a vat-level method of the remote
         ;; machine, right?
         #;[(? mactor:remote-promise? mactor)
            'TODO])]

      ;; TODO: sturdy refr support goes here!
      [(? sturdy-refr?)
       (error "Sturdy refrs not supported yet :(")]

      ;; Anything else?  Well, it's just some value then, we can
      ;; immediately consider that the fulfillment (similar to if it
      ;; were encased).
      [val
       (handle-fulfilled val)])

    ;; Unless an error was thrown, we now should return the promise
    ;; we made.
    (or return-promise (void)))

  (define (get-internals)
    (list actormap to-near to-far))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))


;;; syscall external functions
;;; ==========================

(define <-np
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<-np to-refr args))))

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<- to-refr args))))

(define call
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args 'call to-refr args))))
;; an alias
(define $ call)

(define spawn
  (make-keyword-procedure
   (lambda (kws kw-args constructor . args)
     (define sys (get-syscaller-or-die))
     (sys 'spawn constructor kws kw-args args))))

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
      [(? refr?)
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


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr kws kw-args args
                        #:reckless? [reckless? #f])
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (keyword-apply sys kws kw-args 'call to-refr args))
     (apply values result-val
            (get-sys-internals)))))  ; actormap to-near to-far

(define actormap-turn
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define new-actormap
       (make-transactormap actormap))
     (actormap-turn* new-actormap to-refr kws kw-args args))))

;; Note that this does nothing with the messages.
(define actormap-poke!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* (make-transactormap actormap)
                       to-refr kws kw-args args))
     (transactormap-merge! transactormap)
     returned-val)))

(define actormap-reckless-poke!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-refr kws kw-args args))
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-peek
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* (make-transactormap actormap)
                       to-refr kws kw-args args))
     returned-val)))

;; TODO: We might want to return one of the following:
;;   (values ('call-success val) ('resolve-success val)
;;           actormap to-near to-far)
;;   (values ('call-fail problem) ('resolve-fail problem)
;;           actormap to-near to-far)
;;   (values ('call-success val) #f  ; there was nothing to resolve
;;           actormap to-near to-far)
;; Mix and match the fail/success
(define (actormap-turn-message actormap msg
                               #:display-errors? [display-errors? #t])
  ;; TODO: Kuldgily reimplements part of actormap-turn*... maybe
  ;; there's some opportunity to combine things, dunno.
  (call-with-fresh-syscaller
   (make-transactormap actormap)
   (lambda (sys get-sys-internals)
     (define call-result #f)
     (define result-val (void))
     (with-handlers ([exn:fail?
                      (lambda (err)
                        (when display-errors?
                          (displayln ";; === While attempting to send message: ==="
                                     (current-error-port))
                          ;; TODO: Display the message here
                          ((error-display-handler) (exn-message err) err))
                        (set! call-result
                              `(fail ,err)))])
       (set! result-val
             (sys 'handle-message msg))
       (set! call-result
             `(success ,result-val)))

     (define resolve-me
       (message-resolve-me msg))
     (match (get-sys-internals)
       [(list new-actormap to-near to-far)
        (when resolve-me
          (define resolve-message
            (match call-result
              [(list 'success val)
               (message resolve-me #f '() '() (list 'fulfill val))]
              [(list 'fail err)
               (message resolve-me #f '() '() (list 'break err))]))
          ;; TODO: Handle promises that aren't live refrs
          (define resolve-me-in-same-vat?
            (eq? (live-refr-vat-connector resolve-me)
                 (actormap-vat-connector actormap)))
          (if resolve-me-in-same-vat?
              (set! to-near (cons resolve-message to-near))
              (set! to-far (cons resolve-message to-far))))
        ;; TODO: Isn't result-val redundant?  It's in the call-result...
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
                        #:display-errors? [display-errors? #t])
  (define (cons-if-non-empty a d)
    (cond
      [(null? a) d]
      [(null? d) a]
      [else (cons a d)]))
  (match messages
    [(? message? message)
     (define-values (call-result new-am to-near to-far)
       (actormap-turn-message actormap messages
                              #:display-errors? display-errors?))
     (values new-am to-near to-far)]
    ['()
     (values actormap '() '())]
    [(? pair? message-list)
     (for/fold ([actormap actormap]
                [to-near '()]
                [to-far '()])
               ([msg (reverse messages)])
       (define-values (new-actormap new-to-near new-to-far)
         (actormap-churn actormap msg
                         #:display-errors? display-errors?))
       (values new-actormap
               (cons-if-non-empty new-to-near to-near)
               (cons-if-non-empty new-to-far to-far)))]))

;; Start up an actormap and run until no more messages are left.
;; Not really used in combination with hives; this is mainly
;; to make some simpler patterns easier to test.
;; TODO: Can we generalize/parameterize this in such a way that
;; vats and other designs can be built on top of it?
(define (actormap-full-run! actormap thunk
                            #:display-errors? [display-errors? #t])
  (define-values (_val new-am to-near to-far)
    (actormap-run* actormap thunk))
  (transactormap-merge! new-am)
  (let lp ([messages to-near])
    (define-values (new-am to-near to-far)
      (actormap-churn actormap messages
                      #:display-errors? display-errors?))
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
                          kws kw-args args)
  (define debug-name
    (object-name actor-constructor))
  (define vat-connector
    (actormap-vat-connector actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (define actor-handler
    (keyword-apply actor-constructor kws kw-args become args))
  (match actor-handler
    ;; New procedure, so let's set it
    [(? procedure?)
     (define actor-refr
       (make-live-refr debug-name vat-connector))
     (actormap-set! actormap actor-refr
                    (mactor:local-actor actor-handler
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
   (lambda (kws kw-args actormap actor-constructor . args)
     (define new-actormap
       (make-transactormap actormap))
     (call-with-fresh-syscaller
      new-actormap
      (lambda (sys get-sys-internals)
        (define actor-refr
          (actormap-spawn!* new-actormap actor-constructor
                            kws kw-args args))
        (values actor-refr new-actormap))))))

(define actormap-spawn!
  (make-keyword-procedure
   (lambda (kws kw-args actormap actor-constructor . args)
     (define new-actormap
       (make-transactormap actormap))
     (define actor-refr
       (call-with-fresh-syscaller
        new-actormap
        (lambda (sys get-sys-internals)
          (actormap-spawn!* new-actormap actor-constructor
                            kws kw-args args))))
     (transactormap-merge! new-actormap)
     actor-refr)))

(define (actormap-spawn-mactor! actormap mactor [debug-name #f])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (make-live-refr debug-name vat-connector))
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

;; TODO: Should this return multiple values to its continuation
;;   or do so as a list?  We might be able to speed things up
;;   by not doing the destructuring.
(define (spawn-promise-values)
  (define-values (sealer unsealer tm?)
    (make-sealer-triplet 'fulfill-promise))
  (define sys (get-syscaller-or-die))
  (define promise
    (sys 'spawn-mactor
         (mactor:local-promise '() unsealer tm?)
         'promised))
  ;; I guess the alternatives to responding with false on
  ;; attempting to re-resolve are:
  ;;  - throw an error
  ;;  - just return void regardless
  (define already-resolved
    (lambda _ #f))
  (define (^resolver bcom)
    (match-lambda*
      [(list 'fulfill val)
       (define sys (get-syscaller-or-die))
       (sys 'fulfill-promise promise (sealer val))
       (bcom already-resolved)]
      [(list 'break problem)
       (define sys (get-syscaller-or-die))
       (sys 'break-promise promise (sealer problem))
       (bcom already-resolved)]))
  (define resolver
    (spawn ^resolver))
  (values promise resolver))

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
   "Promise resolves to symlink"
   (mactor:symlink? (whactormap-ref am bob-vow)))
  (test-equal?
   "Resolved symlink acts as what it resolves to"
   (actormap-peek am bob-vow)
   "Hi, I'm bob!")
  (check-not-exn
   (lambda ()
     (actormap-poke! am bob-vow "Hi, I'm bobby!")))
  (test-equal?
   "Resolved symlink can change original"
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
     regardless-also-ran?)))
