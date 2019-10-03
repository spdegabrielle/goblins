#lang racket/base

;;; Exports
;;; =======

(require racket/contract
         racket/set
         "utils/simple-sealers.rkt")

;; Refrs
;; Same as E refs, but -ref is already meaningful in scheme
(provide refr?
         live-refr?
         sturdy-refr?)

;; The making-and-modifying actormap functions
(provide make-whactormap
         ;; alias of make-actormap
         (rename-out [make-whactormap make-actormap])
         whactormap?
         whactormap-ref
         whactormap-set!

         snapshot-whactormap hasheq->whactormap

         transactormap-set! transactormap-ref

         make-transactormap
         transactormap?
         transactormap-parent
         transactormap-delta
         transactormap-merged?
         transactormap-merge!

         actormap?

         actormap-ref
         actormap-set!)

;; The operating-on-actormap main functions
(provide actormap-turn
         actormap-poke!
         actormap-peek
         actormap-extract
         actormap-turn-message

         actormap-run
         actormap-run*
         actormap-run!

         ;; preventing awkwardness: we don't want actor handlers
         ;; to be actor refrs
         ;; TODO: But is this a problem?  Vats are accessing these procedures
         ;;   through these contracts, which almost certainly add slowdown.
         ;;   But the Vat code is more or less "trusted" to be have right.
         (contract-out
          [actormap-spawn
           (->* [any/c
                 (and/c procedure?
                        (not/c refr?))]
                [(or/c #f symbol? string?)]
                (values any/c any/c))]
          [actormap-spawn!
           (->* [any/c (and/c procedure?
                              (not/c refr?))]
                [(or/c #f symbol? string?)]
                any/c)])

         call
         (contract-out
          [spawn
           (->* [(and/c procedure?
                        (not/c refr?))]
                [(or/c #f symbol? string?)]
                any/c)])
         (contract-out
          [on
           (->* [live-refr?]
                [(or/c #f live-refr? procedure?)
                 #:catch (or/c #f live-refr? procedure?)
                 #:return-promise? boolean?]
                any/c)])
         <- <-p
         extract)

;; Cells

(provide make-cell
         spawn-cell
         cell->read-only
         cell->write-only)

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

(struct refr ()
  #:property prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args this . args)
     (keyword-apply call kws kw-args this args))))

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
  (handler become become-unsealer become?))

;; Once encased, always encased.
;; TODO: Maybe we don't need mactors for this.  Maybe anything that's
;;   not a mactor is basically "encased"?  But having an official
;;   mactor type gives us a clearer answer I guess.
(struct mactor:encased mactor (val))
(struct mactor:remote-actor mactor:remote (vat-connid))
(struct mactor:symlink mactor (link-to-refr))
;; Once broken, always broken.
(struct mactor:broken mactor (problem))

;;; Eventual things
;;; ---------------
(struct mactor:local-promise mactor:local
  (listeners resolver-unsealer resolver-tm?))
(struct mactor:remote-promise mactor (vat-connid))

;; TODO: Whatever procedure we make for these we need to
;;   operate with a refr indirection; we won't have the mactor
#;(define local-refr?
  (procedure-rename mactor:local? 'local-refr?))
#;(define far-refr?
  (procedure-rename mactor:remote? 'far-refr?))


;;; "Become" special sealers
;;; ========================

(define (make-become-sealer-triplet)
  (define-values (struct:seal make-seal sealed? seal-ref seal-set!)
    (make-struct-type 'become #f 2 0))
  (define (become handler [return-val (void)])
    (make-seal handler return-val))
  (define unseal-handler
    (make-struct-field-accessor seal-ref 0))
  (define unseal-return-val
    (make-struct-field-accessor seal-ref 1))
  (define (unseal sealed-become)
    (values (unseal-handler sealed-become)
            (unseal-return-val sealed-become)))
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
   (define (actormap-set! whactormap key [dflt #f])
     (whactormap-set! whactormap key dflt))
   (define (actormap-vat-connector whactormap)
     (whactormap-vat-connector whactormap))])

(define (make-whactormap #:vat-connector [vat-connector #f])
  (whactormap (make-weak-hasheq) vat-connector))
(define (whactormap-ref whactormap key [dflt #f])
  (define val
    (hash-ref (whactormap-wht whactormap) key #f))
  ;; TODO: we should use the retain argument instead, once that becomes available
  ;;   https://github.com/racket/racket/commit/99feebf070d0dd3e62c697814e0a42508f7995ee
  (or (and val (match (ephemeron-value val key)
                 ;; workaround until retain-v becomes broadly available
                 [(? (lambda (v) (eq? key v)))
                  #f]
                 [result result]))
      dflt))

(define (whactormap-set! whactormap key val)
  (hash-set! (whactormap-wht whactormap)
             key (make-ephemeron key val)))

(define (snapshot-whactormap whactormap)
  (for/fold ([new-hasheq #hasheq()])
            ([(key val) (whactormap-wht whactormap)])
    (hash-set new-hasheq key val)))

(define (hasheq->whactormap ht)
  (define whactormap (make-whactormap))
  (for ([(key val) ht])
    (whactormap-set! whactormap key val))
  whactormap)

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

(define/contract (make-transactormap parent
                                     [vat-connector
                                      (actormap-vat-connector parent)])
  (->* [actormap?] [(or/c #f procedure?)] any/c)
  (_make-transactormap parent (make-hasheq) #f
                       vat-connector))

(define (transactormap-ref transactormap key [dflt #f])
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-ref on merged transactormap"))
  (or (hash-ref (transactormap-delta transactormap)
                key #f)
      (let ([parent (transactormap-parent transactormap)])
        (match parent
          [(? transactormap?)
           (transactormap-ref parent key #f)]
          [(? whactormap?)
           (whactormap-ref parent key #f)]))
      dflt))

(define/contract (transactormap-set! transactormap key val)
  (-> transactormap? live-refr? any/c any/c)
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (hash-set! (transactormap-delta transactormap)
             key val)
  (void))

;; Serves two functions:
;;  - to extract the root weak-hasheq
;;  - to merge this transaction on top of the weak-hasheq

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
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

(define (actormap-symlink-ref actormap refr-id)
  (let lp ([refr-id refr-id]
           [seen (seteq)])
    (when (set-member? seen refr-id)
      (error "Cycle in mactor symlinks"))
    (match (actormap-ref actormap refr-id #f)
      [(? mactor:symlink? mactor)
       (lp (mactor:symlink-link-to-refr mactor)
           (set-add seen refr-id))]
      [#f (error "no actor with this id")]
      [mactor (values refr-id mactor)])))

(define (fresh-syscaller prev-actormap)
  (define vat-connector
    (actormap-vat-connector prev-actormap))
  (define actormap
    (make-transactormap prev-actormap vat-connector))
  (define to-near '())
  (define to-far '())

  (define closed? #f)

  (define this-syscaller
    (make-keyword-procedure
     (lambda (kws kw-args method-id . args)
       (when closed?
         (error "Sorry, this syscaller is closed for business!"))
       (define method
         (match method-id
           ['call _call]
           ['spawn _spawn]
           ['spawn-mactor spawn-mactor]
           ['fulfill-promise promise-fulfill]
           ['break-promise promise-break]
           ['<- _<-]
           ['<-p _<-p]
           ['on _on]
           ['extract _extract]
           ['vat-connector get-vat-connector]
           [_ (error "invalid syscaller method")]))
       (keyword-apply method kws kw-args args))))

  (define (get-vat-connector)
    vat-connector)

  ;; call actor's handler
  (define _call
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       (define (raise-not-callable)
         (error 'not-callable
                "Actor immediate calls restricted to near-refrs and encased values"))
       ;; Restrict to live-refrs which appear to have the same
       ;; vat-connector as us
       (unless (and (live-refr? to-refr)
                    (eq? (live-refr-vat-connector to-refr)
                         vat-connector))
         (raise-not-callable))

       (define-values (update-refr mactor)
         (actormap-symlink-ref actormap to-refr))
       (match mactor
         [(? mactor:local-actor?)
          (define actor-handler
            (mactor:local-actor-handler mactor))
          (define result
            (keyword-apply actor-handler kws kw-args
                           (mactor:local-actor-become mactor) args))

          ;; I guess watching for this guarantees that an immediate call
          ;; against a local actor will not be tail recursive.
          ;; TODO: We need to document that.
          (define-values (new-handler return-val)
            (match result
              [(? (mactor:local-actor-become? mactor))
               ((mactor:local-actor-become-unsealer mactor) result)]
              [_ (values #f result)]))

          ;; if a new handler for this actor was specified,
          ;; let's replace it
          (when new-handler
            (transactormap-set! actormap update-refr
                                (mactor:local-actor
                                 new-handler
                                 (mactor:local-actor-become mactor)
                                 (mactor:local-actor-become-unsealer mactor)
                                 (mactor:local-actor-become? mactor))))

          return-val]
         [(? mactor:encased?)
          (mactor:encased-val mactor)]
         [_ (raise-not-callable)]))))

  ;; spawn a new actor
  (define (_spawn actor-handler
                  [debug-name (object-name actor-handler)])
    (actormap-spawn! actormap actor-handler debug-name))

  (define (spawn-mactor mactor [debug-name #f])
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (promise-fulfill promise-id sealed-val)
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

       ;; Now we "become" that value!
       (match val
         ;; It's a reference now, so let's set up a symlink
         [(? refr?)
          ;; for efficiency, let's make it as direct of a symlink
          ;; as possible
          (define link-to
            (let lp ([refr-id val]
                     [seen (seteq)])
              (when (set-member? seen refr-id)
                (error "Cycle in mactor symlinks"))
              ;; TODO: deal with far refrs
              (match (actormap-ref actormap refr-id)
                [(? mactor:symlink? mactor)
                 (lp (mactor:symlink-link-to-refr mactor)
                     (set-add seen refr-id))]
                [#f (error "no actor with this id")]
                ;; ok we found a non-symlink refr
                [_ refr-id])))
          (actormap-set! actormap promise-id
                              (mactor:symlink link-to))]
         ;; Must be something else then.  Guess we'd better
         ;; encase it.
         [_ (actormap-set! actormap promise-id
                                (mactor:encased val))])

       ;; Inform all listeners of the resolution
       (for ([listener (mactor:local-promise-listeners promise-mactor)])
         (<- listener 'fulfill val))]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a local-promise")]))

  (define (promise-break promise-id sealed-problem)
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
         (<- listener 'break problem))]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a local-promise")]))

  ;; helper to the become two methods
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
       (set! to-far (cons new-message to-far))]))

  (define _<-
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       (_send-message kws kw-args to-refr #f args)
       (void))))

  (define _<-p
    (make-keyword-procedure
     (lambda (kws kw-args to-refr . args)
       (match-define (list promise resolver)
         (spawn-promise-pair))
       (_send-message kws kw-args to-refr resolver args)
       promise)))

  (define (_on id-refr [on-fulfilled #f]
               #:catch [on-broken #f]
               #:finally [on-finally #f]
               #:return-promise? [return-promise? #f])
    #;(unless (local? id-refr)
      (error "on only works for local objects"))
    (define-values (subscribe-refr mactor)
      (actormap-symlink-ref actormap id-refr))
    ;; Alternate design for these (and the first I implemented) is to
    ;; actually spawn on-finally and on-fulfilled actors and call
    ;; them.  That might be better if we did add coroutines.
    ;; More on this issue and dynamic-wind:
    ;; https://groups.google.com/d/msg/racket-users/-NHDOdo6DAk/4NcTa89sIpMJ
    (define (call-on-fulfilled val)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (when on-fulfilled
            (on-fulfilled val)))
        call-on-finally))
    (define (call-on-broken problem)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (when on-broken
            (on-broken problem)))
        call-on-finally))
    (define (call-on-finally)
      (when on-finally
        (on-finally)))
    (match mactor
      [(mactor:local-promise listeners r-unsealer r-tm?)
       (match-define (list return-promise return-p-resolver)
         (if return-promise?
             (spawn-promise-pair)
             (list #f #f)))
       (define on-listener
         ;; using _spawn here saves a very minor round
         ;; trip which we can't do in the on-fulfilled
         ;; ones because they'll be in a new syscaller
         (_spawn
          (match-lambda*
            [(list bcom 'fulfill val)
             (define fulfilled-response-val
               (call-on-fulfilled val))
             (when return-promise?
               (<- return-p-resolver 'fulfill fulfilled-response-val))
             ;; Not sure if we do need to, or it is useful to,
             ;; return this, or if we should just return void.
             ;; I don't think it hurts?
             fulfilled-response-val]
            [(list bcom 'break problem)
             (when return-promise?
               (<- return-p-resolver 'break problem))
             (call-on-broken problem)])))
       (define new-listeners
         (cons on-listener listeners))
       (actormap-set! actormap id-refr
                      (mactor:local-promise new-listeners
                                            r-unsealer r-tm?))
       (if return-promise?
           return-promise
           (void))]
      [(? mactor:broken? mactor)
       (call-on-broken (mactor:broken-problem mactor))
       (call-on-finally)]
      [(? mactor:encased? mactor)
       (call-on-fulfilled (mactor:encased-val mactor))
       (call-on-finally)]
      [(? (or/c mactor:remote? mactor:local-actor?) mactor)
       (call-on-fulfilled subscribe-refr)
       (call-on-finally)]
      ;; This involves invoking a vat-level method of the remote
      ;; machine, right?
      [(? mactor:remote-promise? mactor)
       'TODO]))

  (define (_extract id-refr)
    (actormap-extract actormap id-refr))

  (define (get-internals)
    (list actormap to-near to-far))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))


;;; syscall external functions
;;; ==========================

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<- to-refr args))))

(define <-p
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<-p to-refr args))))

(define call
  (make-keyword-procedure
   (lambda (kws kw-args to-refr . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args 'call to-refr args))))

(define (spawn actor-handler
               [debug-name (object-name actor-handler)])
  (define sys (get-syscaller-or-die))
  (sys 'spawn actor-handler debug-name))

(define (on id-refr [on-fulfilled #f]
            #:catch [on-broken #f]
            #:finally [on-finally #f]
            #:return-promise? [return-promise? #f])
  (define sys (get-syscaller-or-die))
  (sys 'on id-refr on-fulfilled
       #:catch on-broken
       #:finally on-finally
       #:return-promise? return-promise?))

(define (extract id-refr)
  (define sys (get-syscaller-or-die))
  (sys 'extract id-refr))


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr kws kw-args args)
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
     (actormap-turn* actormap to-refr kws kw-args args))))

;; Note that this does nothing with the messages.
(define actormap-poke!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-refr kws kw-args args))
     (transactormap-merge! transactormap)
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-peek
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-refr . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* actormap to-refr kws kw-args args))
     returned-val)))

(define (actormap-extract actormap id-refr)
  (define-values (_refr mactor)
    (actormap-symlink-ref actormap id-refr))
  (match mactor
    [(? mactor:encased? mactor)
     (mactor:encased-val mactor)]
    [mactor (error "Not an encased val" mactor)]))

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
   actormap
   (lambda (sys get-sys-internals)
     (match-define (message to resolve-me kws kw-vals args)
       msg)
     (unless (live-refr? to)
       (error "Can only perform a turn on a message to local actors"))
     (define call-result #f)
     (define result-val (void))
     (with-handlers ([exn:fail?
                      (lambda (err)
                        (when display-errors?
                          (displayln "=== While attempting to send message: ==="
                                     (current-error-port))
                          ;; TODO: Display the message here
                          ((error-display-handler) (exn-message err) err))
                        (set! call-result
                              `(fail ,err)))])
       (set! result-val
             (keyword-apply sys kws kw-vals 'call to args))
       (set! call-result
             `(success ,result-val)))

     (define resolve-result #f)
     (when resolve-me
       (with-handlers ([exn:fail?
                        (lambda (err)
                          (when display-errors?
                            (displayln "=== While attempting to resolve promise: ==="
                                       (current-error-port))
                            ;; TODO: Display the message here
                            ((error-display-handler) (exn-message err) err))
                          (set! resolve-result
                                `(fail ,err)))])
         (match call-result
           [(list 'success val)
            (keyword-apply sys '() '() 'call
                           resolve-me 'fulfill (list val))]
           [(list 'fail err)
            (keyword-apply sys '() '() 'call
                           resolve-me 'break (list err))])))

     (apply values
            call-result resolve-result
            result-val
            (get-sys-internals)))))  ; actormap to-near to-far

;; The following two are utilities for when you want to check
;; or bootstrap something within an actormap

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val _am _tl _tr)
    (actormap-run* actormap thunk))
  returned-val)

;; like actormap-run but also returns the new actormap, to-near, to-far
(define (actormap-run* actormap thunk)
  (define-values (actor-refr new-actormap)
    (actormap-spawn actormap (lambda (become) (thunk))))
  (define-values (returned-val new-actormap2 to-near to-far)
    (actormap-turn* new-actormap actor-refr '() '() '()))
  (values returned-val new-actormap2 to-near to-far))

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define (actormap-run! actormap thunk)
  (define actor-refr
    (actormap-spawn! actormap (lambda (become) (thunk))))
  (actormap-poke! actormap actor-refr))


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
     (define-values (call-result resolve-result _val new-am to-near to-far)
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

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-handler
                        [debug-name (object-name actor-handler)])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (make-live-refr debug-name vat-connector))
  (define new-actormap
    (make-transactormap actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))

  (transactormap-set! new-actormap actor-refr
                      (mactor:local-actor actor-handler
                                          become become-unseal become?))
  (values actor-refr new-actormap))

(define (actormap-spawn! actormap actor-handler
                         [debug-name (object-name actor-handler)])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (make-live-refr debug-name vat-connector))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (actormap-set! actormap actor-refr
                 (mactor:local-actor actor-handler
                                     become become-unseal become?))
  actor-refr)

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

  (define ((counter n) bcom)
    (bcom (counter (add1 n))
            n))

  ;; can actors update themselves?
  (define ctr-refr
    (actormap-spawn! am (counter 1) 'ctr))
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

  (define (friend-spawner bcom friend-name)
    (define ((a-friend [called-times 0]) bcom)
      (define new-called-times
        (add1 called-times))
      (bcom (a-friend new-called-times)
            (format "Hello!  My name is ~a and I've been called ~a times!"
                    friend-name new-called-times)))
    (spawn (a-friend) 'friend))
  (define fr-spwn (actormap-spawn! am friend-spawner))
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
    (actormap-spawn am (lambda (bcom) 'noncommital)))
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
      (spawn friend-spawner))
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
   "Hello!  My name is brian and I've been called 2 times!"))

;;; Cells
;;; =====

;; A simple turn-mutable cell

(define (make-cell [val #f])
  (case-lambda
    [(bcom) val]
    [(bcom new-val)
     (bcom (make-cell new-val))]))

(define (spawn-cell [val #f])
  (spawn (make-cell val) 'cell))

(define (cell->read-only cell)
  (spawn (lambda () (cell))))

(define (cell->write-only cell)
  (spawn (lambda (new-val) (cell new-val))))

(module+ test
  (define a-cell
    (actormap-spawn! am (make-cell)))
  (test-eq?
   "cell without default value and unset is #f"
   (actormap-peek am a-cell)
   #f)
  (actormap-poke! am a-cell 'foo)
  (test-eq?
   "cell after being set retains value"
   (actormap-peek am a-cell)
   'foo)
  
  (test-eq?
   "cell default values"
   (actormap-peek am (actormap-spawn! am (make-cell 'hello)))
   'hello))

;;; Promises
;;; ========

;; TODO: Should this return multiple values to its continuation
;;   or do so as a list?  We might be able to speed things up
;;   by not doing the destructuring.
(define (spawn-promise-pair)
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
  (define resolver
    (spawn
     (match-lambda*
       [(list bcom 'fulfill val)
        (define sys (get-syscaller-or-die))
        (sys 'fulfill-promise promise (sealer val))
        (bcom already-resolved)]
       [(list bcom 'break problem)
        (define sys (get-syscaller-or-die))
        (sys 'break-promise promise (sealer problem))
        (bcom already-resolved)])
     'resolver))
  (list promise resolver))

(module+ test
  (define bob (actormap-spawn! am (make-cell "Hi, I'm bob!")))
  (match-define (list bob-vow bob-resolver)
    (actormap-run! am spawn-promise-pair))
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


  (match-define (list encase-me-vow encase-me-resolver)
    (actormap-run! am spawn-promise-pair))
  (actormap-poke! am encase-me-resolver 'fulfill 'encase-me)
  (test-eq?
   "actormap-extract on encased value"
   (actormap-extract am encase-me-vow)
   'encase-me)
  (test-exn
   "actormap-extract on non-encased value throws exception"
   #rx"^Not an encased val"
   (lambda ()
     (actormap-extract am bob)))
  (test-eq?
   "extract procedure works"
   (actormap-run am (lambda () (extract encase-me-vow)))
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
         (define fulfilled-cell (spawn-cell #f))
         (define broken-cell (spawn-cell #f))
         (define finally-cell (spawn-cell #f))
         (match-define (list a-vow a-resolver)
           (spawn-promise-pair))
         (on a-vow
             (lambda args
               (fulfilled-cell args))
             #:catch
             (lambda args
               (broken-cell args))
             #:finally
             (lambda ()
               (finally-cell #t)))
         (apply <- a-resolver resolve-args)
         (list fulfilled-cell broken-cell finally-cell))))
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
          (on (<-p (spawn (lambda _ 'i-am-foo)))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e))))))
    (test-equal?
     "<-p returns a listen'able promise"
     what-i-got
     '(yeah i-am-foo)))

  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (on (<-p (spawn (lambda _ (error "I am error"))))
              (lambda (v)
                (set! what-i-got `(yeah ,v)))
              #:catch
              (lambda (e)
                (set! what-i-got `(oh-no ,e)))))
     #:display-errors? #f)
    (test-equal?
     "<-p promise breaks as expected"
     (car what-i-got)
     'oh-no))
  
  (let ([what-i-got #f])
    (actormap-full-run!
     am (lambda ()
          (define foo (spawn (lambda _ 'i-am-foo)))
          (on (<-p (<-p (spawn (lambda _ foo))))
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
          (define fatal-foo (spawn (lambda _ (error "I am error"))))
          (on (<-p (<-p (spawn (lambda _ fatal-foo))))
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
   "Passing #:return-promise? to `on` returns a promise that is resolved"
   (actormap-extract
    am
    (actormap-full-run!
     am (lambda ()
          (define doubler (spawn (lambda (bcom x)
                                   (* x 2))))
          (define the-on-promise
            (on (<-p doubler 3)
                (lambda (x)
                  (format "got: ~a" x))
                #:catch
                (lambda (e)
                  "uhoh")
                #:return-promise? #t))
          the-on-promise)))
   "got: 6"))
