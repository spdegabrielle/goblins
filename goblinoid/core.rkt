#lang racket/base

;;; Exports
;;; =======

(require racket/contract
         racket/set
         "utils/simple-sealers.rkt")

;; Refs
;; TODO: We might want to rename these to ids
(provide ref?
         live-ref?
         sturdy-ref?)

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
         ;; to be actor refs
         (contract-out
          [actormap-spawn
           (->* [any/c
                 (and/c procedure?
                        (not/c ref?))]
                [(or/c #f symbol? string?)]
                (values any/c any/c))]
          [actormap-spawn!
           (->* [any/c (and/c procedure?
                              (not/c ref?))]
                [(or/c #f symbol? string?)]
                any/c)])

         (rename-out [make-next next])
         ;; next? next-return-val next-handler

         call
         (contract-out
          [spawn
           (->* [(and/c procedure?
                        (not/c ref?))]
                [(or/c #f symbol? string?)]
                any/c)])
         (contract-out
          [on
           (->* [live-ref?]
                [(or/c #f live-ref? procedure?)
                 #:catch (or/c #f live-ref? procedure?)
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


;;; Imports
;;; =======

(require "message.rkt"
         "hash-contracts.rkt"
         racket/match
         racket/generic)


;;; Refs
;;; ====

(struct ref ()
  #:property prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args this . args)
     (keyword-apply call kws kw-args this args))))

(struct live-ref ref (debug-name)
  #:constructor-name _make-live-ref
  #:methods gen:custom-write
  [(define (write-proc ref port mode)
     (define str-to-write
       (match (live-ref-debug-name ref)
         [#f "#<live-ref>"]
         ;; TODO: Do we need to do escaping?
         [debug-name (format "#<live-ref ~a>" debug-name)]))
     (write-string str-to-write port))])

(define (make-live-ref [debug-name #f])
  (_make-live-ref debug-name))

(struct sturdy-ref ref (swiss-num vat-id conn-hints))

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

(struct mactor ())

;;; Resolved things
;; once a near ref, always a near ref.
(struct mactor:near mactor (handler))
;; Once encased, always encased.
;; TODO: Maybe we don't need mactors for this.  Maybe anything that's
;;   not a mactor is basically "encased"?  But having an official
;;   mactor type gives us a clearer answer I guess.
(struct mactor:encased mactor (val))
(struct mactor:far mactor (vat-connid))
(struct mactor:symlink mactor (link-to-ref))
;; Once broken, always broken.
(struct mactor:broken mactor (problem))

;; Eventual things
(struct mactor:near-promise mactor (listeners resolver-unsealer resolver-tm?))
(struct mactor:far-promise mactor (vat-connid))


;;; Actormaps, whactormaps and transactormaps
;;; =========================================

;; An transactormap is a transactional structure used by the
;; actormap turn system

;; Uses ephemerons to allow for self-referencing collection...
;; hopefully works right.
(define-generics actormap
  (actormap-ref actormap key [dflt])
  (actormap-set! actormap key val))

(struct whactormap (wht) ; weak hash table
  #:methods gen:actormap
  [(define (actormap-ref whactormap key [dflt #f])
     (whactormap-ref whactormap key dflt))
   (define (actormap-set! whactormap key [dflt #f])
     (whactormap-set! whactormap key dflt))])

(define (make-whactormap)
  (whactormap (make-weak-hasheq)))
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

(struct transactormap (parent delta [merged? #:mutable])
  #:constructor-name _make-transactormap
  #:methods gen:actormap
  [(define (actormap-ref transactormap key [dflt #f])
     (transactormap-ref transactormap key dflt))
   (define (actormap-set! transactormap key val)
     (transactormap-set! transactormap key val))])

#;(define actormap?
  (or/c transactormap? actormap?))

(define/contract (make-transactormap parent)
  (-> actormap? any/c)
  (_make-transactormap parent (make-hasheq) #f))

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
  (-> transactormap? live-ref? any/c any/c)
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
  (define beeper-ref (make-live-ref 'beeper))
  (define (beeper-proc . args)
    'beep)
  (whactormap-set! actormap-base beeper-ref beeper-proc)
  (define booper-ref (make-live-ref 'booper))
  (define (booper-proc . args)
    'boop)
  (whactormap-set! actormap-base booper-ref booper-proc)
  (define blepper-ref (make-live-ref 'blepper))
  (define (blepper-proc . args)
    'blep)
  (whactormap-set! actormap-base blepper-ref blepper-proc)

  (define tam1
    (make-transactormap actormap-base))
  (define bipper-ref (make-live-ref 'bipper))
  (define (bipper-proc . args)
    'bippity)
  (transactormap-set! tam1 bipper-ref bipper-proc)
  (define (booper-proc2 . args)
    'boop2)
  (transactormap-set! tam1 booper-ref booper-proc2)
  (define (blepper-proc2 . args)
    'blep2)
  (transactormap-set! tam1 blepper-ref blepper-proc2)
  (check-eq? (transactormap-ref tam1 bipper-ref)
             bipper-proc)
  (check-eq? (transactormap-ref tam1 beeper-ref)
             beeper-proc)
  (check-eq? (transactormap-ref tam1 booper-ref)
             booper-proc2)
  (check-eq? (transactormap-ref tam1 blepper-ref)
             blepper-proc2)
  (check-eq? (whactormap-ref actormap-base booper-ref #f)
             booper-proc)
  (check-false (transactormap-merged? tam1))

  (define tam2
    (make-transactormap tam1))

  (define boppiter-ref (make-live-ref 'boppiter))
  (define (boppiter-proc . args)
    'boppitty)
  (transactormap-set! tam2 boppiter-ref boppiter-proc)
  (define (booper-proc3 . args)
    'boop3)
  (transactormap-set! tam2 booper-ref booper-proc3)

  (check-eq? (transactormap-ref tam2 beeper-ref)
             beeper-proc)
  (check-eq? (transactormap-ref tam2 booper-ref)
             booper-proc3)
  (check-eq? (transactormap-ref tam2 bipper-ref)
             bipper-proc)
  (check-eq? (transactormap-ref tam2 boppiter-ref)
             boppiter-proc)
  (check-eq? (transactormap-ref tam2 blepper-ref)
             blepper-proc2)
  (check-eq? (whactormap-ref actormap-base booper-ref #f)
             booper-proc)
  (check-eq? (whactormap-ref actormap-base boppiter-ref #f)
             #f)
  (check-false (transactormap-merged? tam2))

  (transactormap-merge! tam2)
  (check-true (transactormap-merged? tam2))
  (check-true (transactormap-merged? tam1))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam2 beeper-ref)))
  (check-exn any/c
             (lambda ()
               (transactormap-ref tam1 beeper-ref)))
  (check-exn any/c
             (lambda ()
               (transactormap-set! tam1 beeper-ref
                                   (lambda _ 'whatever))))

  (check-eq? (whactormap-ref actormap-base beeper-ref)
             beeper-proc)
  (check-eq? (whactormap-ref actormap-base booper-ref)
             booper-proc3)
  (check-eq? (whactormap-ref actormap-base bipper-ref)
             bipper-proc)
  (check-eq? (whactormap-ref actormap-base boppiter-ref)
             boppiter-proc)
  (check-eq? (whactormap-ref actormap-base blepper-ref)
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

(define (actormap-symlink-ref actormap ref-id)
  (let lp ([ref-id ref-id]
           [seen (seteq)])
    (when (set-member? seen ref-id)
      (error "Cycle in mactor symlinks"))
    (match (actormap-ref actormap ref-id #f)
      [(? mactor:symlink? mactor)
       (lp (mactor:symlink-link-to-ref mactor)
           (set-add seen ref-id))]
      [#f (error "no actor with this id")]
      [mactor (values ref-id mactor)])))

(define (fresh-syscaller prev-actormap)
  (define actormap
    (make-transactormap prev-actormap))
  (define to-local '())
  (define to-remote '())

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
           [_ (error "invalid syscaller method")]))
       (keyword-apply method kws kw-args args))))

  ;; call actor's handler
  (define _call
    (make-keyword-procedure
     (lambda (kws kw-args to-ref . args)
       (define-values (update-ref mactor)
         (actormap-symlink-ref actormap to-ref))
       (match mactor
         [(? mactor:near?)
          (define actor-handler
            (mactor:near-handler mactor))
          (define result
            (keyword-apply actor-handler kws kw-args args))

          ;; I guess watching for this guarantees that an immediate call
          ;; against a local actor will not be tail recursive.
          ;; TODO: We need to document that.
          (define-values (return-val new-handler)
            (match result
              [(? next?)
               (values (next-return-val result)
                       (next-handler result))]
              [_ (values result #f)]))

          ;; if a new handler for this actor was specified,
          ;; let's replace it
          (when new-handler
            (transactormap-set! actormap update-ref
                                (mactor:near new-handler)))

          return-val]
         [(? mactor:encased?)
          (mactor:encased-val mactor)]
         [_
          (error "Actor immediate calls restricted to near-refs and encased values")]))))

  ;; spawn a new actor
  (define (_spawn actor-handler
                  [debug-name (object-name actor-handler)])
    (actormap-spawn! actormap actor-handler debug-name))

  (define (spawn-mactor mactor [debug-name #f])
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (promise-fulfill promise-id sealed-val)
    (match (actormap-ref actormap promise-id #f)
      [(? mactor:near-promise? promise-mactor)
       (define resolver-tm?
         (mactor:near-promise-resolver-tm? promise-mactor))
       (define resolver-unsealer
         (mactor:near-promise-resolver-unsealer promise-mactor))
       ;; Is this a valid resolution?
       (unless (resolver-tm? sealed-val)
         (error "Resolution sealed with wrong trademark!"))
       (define val
         (resolver-unsealer sealed-val))

       ;; Now we "become" that value!
       (match val
         ;; It's a reference now, so let's set up a symlink
         [(? ref?)
          ;; for efficiency, let's make it as direct of a symlink
          ;; as possible
          (define link-to
            (let lp ([ref-id val]
                     [seen (seteq)])
              (when (set-member? seen ref-id)
                (error "Cycle in mactor symlinks"))
              ;; TODO: deal with far refs
              (match (actormap-ref actormap ref-id)
                [(? mactor:symlink? mactor)
                 (lp (mactor:symlink-link-to-ref mactor)
                     (set-add seen ref-id))]
                [#f (error "no actor with this id")]
                ;; ok we found a non-symlink ref
                [_ ref-id])))
          (actormap-set! actormap promise-id
                              (mactor:symlink link-to))]
         ;; Must be something else then.  Guess we'd better
         ;; encase it.
         [_ (actormap-set! actormap promise-id
                                (mactor:encased val))])

       ;; Inform all listeners of the resolution
       (for ([listener (mactor:near-promise-listeners promise-mactor)])
         (<- listener 'fulfill val))]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a near-promise")]))

  (define (promise-break promise-id sealed-problem)
    (match (actormap-ref actormap promise-id #f)
      ;; TODO: Not just near-promise, anything that can
      ;;   break
      [(? mactor:near-promise? promise-mactor)
       (define resolver-tm?
         (mactor:near-promise-resolver-tm? promise-mactor))
       (define resolver-unsealer
         (mactor:near-promise-resolver-unsealer promise-mactor))
       ;; Is this a valid resolution?
       (unless (resolver-tm? sealed-problem)
         (error "Resolution sealed with wrong trademark!"))
       (define problem
         (resolver-unsealer sealed-problem))
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                           (mactor:broken problem))
       ;; Inform all listeners of the resolution
       (for ([listener (mactor:near-promise-listeners promise-mactor)])
         (<- listener 'break problem))]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve a near-promise")]))

  ;; helper to the next two methods
  (define (_send-message kws kw-args actor-ref resolve-me args)
    (define new-message
      (message actor-ref resolve-me kws kw-args args))
    (match actor-ref
      [(? live-ref?)
       (set! to-local (cons new-message to-local))]
      #;[(? far-ref?)
       (set! to-remote (cons new-message to-remote))]))

  (define _<-
    (make-keyword-procedure
     (lambda (kws kw-args actor-ref . args)
       (_send-message kws kw-args actor-ref #f args)
       (void))))

  (define _<-p
    (make-keyword-procedure
     (lambda (kws kw-args actor-ref . args)
       (match-define (list promise resolver)
         (spawn-promise-pair))
       (_send-message kws kw-args actor-ref resolver args)
       promise)))

  (define (_on id-ref [on-fulfilled #f]
               #:catch [on-broken #f]
               #:finally [on-finally #f]
               #:return-promise? [return-promise? #f])
    #;(unless (near? id-ref)
      (error "on only works for near objects"))
    (define-values (subscribe-ref mactor)
      (actormap-symlink-ref actormap id-ref))
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
      [(mactor:near-promise listeners r-unsealer r-tm?)
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
            [(list 'fulfill val)
             (define fulfilled-response-val
               (call-on-fulfilled val))
             (when return-promise?
               (<- return-p-resolver 'fulfill fulfilled-response-val))
             ;; Not sure if we do need to, or it is useful to,
             ;; return this, or if we should just return void.
             ;; I don't think it hurts?
             fulfilled-response-val]
            [(list 'break problem)
             (when return-promise?
               (<- return-p-resolver 'break problem))
             (call-on-broken problem)])))
       (define new-listeners
         (cons on-listener listeners))
       (actormap-set! actormap id-ref
                           (mactor:near-promise new-listeners
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
      [(? (or/c mactor:far? mactor:near?) mactor)
       (call-on-fulfilled subscribe-ref)
       (call-on-finally)]
      ;; This involves invoking a vat-level method of the remote
      ;; machine, right?
      [(? mactor:far-promise? mactor)
       'TODO]))

  (define (_extract id-ref)
    (actormap-extract actormap id-ref))

  (define (get-internals)
    (list actormap to-local to-remote))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))


;;; setting up next handler
;;; =======================

(struct next (handler return-val)
  #:constructor-name _make-next)
(define/contract (make-next handler [return-val (void)])
  (->* [(and/c procedure? (not/c ref?))]
       [(not/c next?)]
       any/c)
  (_make-next handler return-val))

(module+ extra-next
  (provide next next? next-handler next-return-val))


;;; syscall external functions
;;; ==========================

(define <-
  (make-keyword-procedure
   (lambda (kws kw-args to-ref . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<- to-ref args))))

(define <-p
  (make-keyword-procedure
   (lambda (kws kw-args to-ref . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args '<-p to-ref args))))

(define call
  (make-keyword-procedure
   (lambda (kws kw-args to-ref . args)
     (define sys (get-syscaller-or-die))
     (keyword-apply sys kws kw-args 'call to-ref args))))

(define (spawn actor-handler
               [debug-name (object-name actor-handler)])
  (define sys (get-syscaller-or-die))
  (sys 'spawn actor-handler debug-name))

(define (on id-ref [on-fulfilled #f]
            #:catch [on-broken #f]
            #:finally [on-finally #f]
            #:return-promise? [return-promise? #f])
  (define sys (get-syscaller-or-die))
  (sys 'on id-ref on-fulfilled
       #:catch on-broken
       #:finally on-finally
       #:return-promise? return-promise?))

(define (extract id-ref)
  (define sys (get-syscaller-or-die))
  (sys 'extract id-ref))


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-ref kws kw-args args)
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (keyword-apply sys kws kw-args 'call to-ref args))
     (apply values result-val
            (get-sys-internals)))))  ; actormap to-local to-remote

(define actormap-turn
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (actormap-turn* actormap to-ref kws kw-args args))))

;; Note that this does nothing with the messages.
(define actormap-poke!
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val transactormap _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     (transactormap-merge! transactormap)
     returned-val)))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define actormap-peek
  (make-keyword-procedure
   (lambda (kws kw-args actormap to-ref . args)
     (define-values (returned-val _am _tl _tr)
       (actormap-turn* actormap to-ref kws kw-args args))
     returned-val)))

(define (actormap-extract actormap id-ref)
  (define-values (_ref mactor)
    (actormap-symlink-ref actormap id-ref))
  (match mactor
    [(? mactor:encased? mactor)
     (mactor:encased-val mactor)]
    [mactor (error "Not an encased val" mactor)]))

;; TODO: We might want to return one of the following:
;;   (values ('call-success val) ('resolve-success val)
;;           actormap to-local to-remote)
;;   (values ('call-fail problem) ('resolve-fail problem)
;;           actormap to-local to-remote)
;;   (values ('call-success val) #f  ; there was nothing to resolve
;;           actormap to-local to-remote)
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
     (unless (live-ref? to)
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
            (get-sys-internals)))))  ; actormap to-local to-remote

;; The following two are utilities for when you want to check
;; or bootstrap something within an actormap

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val _am _tl _tr)
    (actormap-run* actormap thunk))
  returned-val)

;; like actormap-run but also returns the new actormap, to-local, to-remote
(define (actormap-run* actormap thunk)
  (define-values (actor-ref new-actormap)
    (actormap-spawn actormap thunk))
  (define-values (returned-val new-actormap2 to-local to-remote)
    (actormap-turn* new-actormap actor-ref '() '() '()))
  (values returned-val new-actormap2 to-local to-remote))

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define (actormap-run! actormap thunk)
  (define actor-ref
    (actormap-spawn! actormap thunk))
  (actormap-poke! actormap actor-ref))


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
     (define-values (call-result resolve-result _val new-am to-local to-remote)
       (actormap-turn-message actormap messages
                              #:display-errors? display-errors?))
     (values new-am to-local to-remote)]
    ['()
     (values actormap '() '())]
    [(? pair? message-list)
     (for/fold ([actormap actormap]
                [to-local '()]
                [to-remote '()])
               ([msg (reverse messages)])
       (define-values (new-actormap new-to-local new-to-remote)
         (actormap-churn actormap msg
                         #:display-errors? display-errors?))
       (values new-actormap
               (cons-if-non-empty new-to-local to-local)
               (cons-if-non-empty new-to-remote to-remote)))]))

;; Start up an actormap and run until no more messages are left.
;; Not really used in combination with hives; this is mainly
;; to make some simpler patterns easier to test.
;; TODO: Can we generalize/parameterize this in such a way that
;; vats and other designs can be built on top of it?
(define (actormap-full-run! actormap thunk
                            #:display-errors? [display-errors? #t])
  (define-values (_val new-am to-local to-remote)
    (actormap-run* actormap thunk))
  (transactormap-merge! new-am)
  (let lp ([messages to-local])
    (define-values (new-am to-local to-remote)
      (actormap-churn actormap messages
                      #:display-errors? display-errors?))
    (when (transactormap? new-am)
      (transactormap-merge! new-am))
    (if (null? to-local)
        (void)
        (lp to-local)))
  _val)


;; Spawning
;; ========

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-handler
                        [debug-name (object-name actor-handler)])
  (define actor-ref
    (make-live-ref debug-name))
  (define new-actormap
    (make-transactormap actormap))
  (transactormap-set! new-actormap actor-ref
                      (mactor:near actor-handler))
  (values actor-ref new-actormap))

(define (actormap-spawn! actormap actor-handler
                         [debug-name (object-name actor-handler)])
  (define actor-ref
    (make-live-ref debug-name))
  (actormap-set! actormap actor-ref
                      (mactor:near actor-handler))
  actor-ref)

(define (actormap-spawn-mactor! actormap mactor [debug-name #f])
  (define actor-ref
    (make-live-ref debug-name))
  (actormap-set! actormap actor-ref mactor)
  actor-ref)

(module+ test
  (require rackunit
           racket/contract)
  (define am (make-whactormap))

  (define ((counter n))
    (make-next (counter (add1 n))
               n))

  ;; can actors update themselves?
  (define ctr-ref
    (actormap-spawn! am (counter 1) 'ctr))
  (define-values (turned-val1 am+ctr1 _to-local _to-remote)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1 1)
  (define-values (turned-val2 am+ctr2 _to-local2 _to-remote2)
    (actormap-turn am+ctr1 ctr-ref))
  (check-eqv? turned-val2 2)

  ;; transaction shouldn't be applied yet
  (define-values (turned-val1-again
                  am+ctr1-again
                  _to-local-again _to-remote-again)
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val1-again 1)

  ;; but now it should be
  (transactormap-merge! am+ctr2)
  (define-values (turned-val3 am+ctr3 _to-local3 _to-remote3)
    ;; observe that we're turning using the "original"
    ;; actormap though!  It should have committed.
    (actormap-turn am ctr-ref))
  (check-eqv? turned-val3 3)

  (define (friend-spawner friend-name)
    (define ((a-friend [called-times 0]))
      (define new-called-times
        (add1 called-times))
      (make-next (a-friend new-called-times)
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

  (define-values (noncommital-ref noncommital-am)
    (actormap-spawn am (lambda () 'noncommital)))
  (check-eq?
   (actormap-peek noncommital-am noncommital-ref)
   'noncommital)
  (check-exn
   any/c
   (lambda () (actormap-peek am noncommital-ref)))

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
    [() val]
    [(new-val)
     (make-next (make-cell new-val))]))

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
         (mactor:near-promise '() unsealer tm?)
         'promised))
  ;; I guess the alternatives to responding with false on
  ;; attempting to re-resolve are:
  ;;  - throw an error
  ;;  - just return void regardless
  (define already-resolved
    (lambda () #f))
  (define resolver
    (spawn
     (match-lambda*
       [(list 'fulfill val)
        (define sys (get-syscaller-or-die))
        (sys 'fulfill-promise promise (sealer val))
        (make-next already-resolved)]
       [(list 'break problem)
        (define sys (get-syscaller-or-die))
        (sys 'break-promise promise (sealer problem))
        (make-next already-resolved)])
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
   "Using `on' against a resolved ref returns that ref"
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
   "Using `on' against a resolved ref returns that ref"
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
          (define doubler (spawn (lambda (x) (* x 2))))
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
