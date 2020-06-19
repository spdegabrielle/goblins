#lang racket/base

(require racket/async-channel
         racket/match
         racket/random
         racket/contract
         racket/set
         "core.rkt"
         (submod "core.rkt" for-captp)
         "message.rkt"
         "vat.rkt"
         "actor-lib/methods.rkt"
         "utils/simple-sealers.rkt"
         "errors.rkt"
         syrup)

(require pk)

;;; Messages

;; A helper... we're going to have a bunch of structs that have a way
;; of being marshalled to and from records.  This helps reduce the
;; overhead of defining those.
(define-syntax-rule (define-recordable-struct struct-id
                      (args ...)
                      struct-marshall-id struct-unmarshall-id)
  (begin (struct struct-id (args ...)
           #:transparent)
         (define struct-marshall-id
           (cons (match-lambda
                   [(struct-id args ...)
                    #t]
                   [_ #f])
                 (match-lambda
                   [(struct-id args ...)
                    (record* (quote struct-id) args ...)])))
         (define struct-unmarshall-id
           (cons (lambda (label)
                   (eq? label (quote struct-id)))
                 struct-id))))

(define-recordable-struct op:bootstrap
  (answer-pos resolve-me-desc)
  marshall::op:bootstrap unmarshall::op:bootstrap)

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-recordable-struct op:deliver-only
  (;; Position in the table for the target
   ;; (sender's imports, reciever's exports)
   to-desc
   ;; Either the method name, or #f if this is a procedure call
   method
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
   args
   kw-args)
  marshall::op:deliver-only unmarshall::op:deliver-only)

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-recordable-struct op:deliver
  (to-desc
   method
   args
   kw-args
   answer-pos
   resolve-me-desc)  ; a resolver, probably an import (though it could be a handoff)
  marshall::op:deliver unmarshall::op:deliver)

(define-recordable-struct op:abort
  (reason)
  marshall::op:abort unmarshall::op:abort)

;; It's unclear whether or not op: listen is really needed at all...
;; So far there's no clear indication that it is, and we might be
;; able to remove this.
(define-recordable-struct op:listen
  (to-desc listener-desc wants-partial?)
  marshall::op:listen unmarshall::op:listen)

(define-recordable-struct desc:import-object
  (pos)
  marshall::desc:import-object unmarshall::desc:import-object)

(define-recordable-struct desc:import-promise
  (pos)
  marshall::desc:import-promise unmarshall::desc:import-promise)

(define (desc:import-pos import-desc)
  (match import-desc
    [(? desc:import-object?)
     (desc:import-object-pos import-desc)]
    [(? desc:import-promise?)
     (desc:import-promise-pos import-desc)]))

(define (desc:import? obj)
  (or (desc:import-object? obj)
      (desc:import-promise? obj)))

;; Whether it's an import or export doesn't really matter as much to
;; the entity exporting as it does to the entity importing
(define-recordable-struct desc:export
  (pos)
  marshall::desc:export unmarshall::desc:export)

;; Something to answer that we haven't seen before.
;; As such, we need to set up both the promise import and this resolver/redirector
(define-recordable-struct desc:answer
  (pos)
  marshall::desc:answer unmarshall::desc:answer)

;; TODO: 3 vat/machine handoff versions (Promise3Desc, Far3Desc)

(define marshallers
  (list marshall::op:bootstrap
        marshall::op:deliver-only
        marshall::op:deliver
        marshall::op:abort
        marshall::op:listen
        marshall::desc:import-object
        marshall::desc:import-promise
        marshall::desc:export
        marshall::desc:answer))

(define unmarshallers
  (list unmarshall::op:bootstrap
        unmarshall::op:deliver-only
        unmarshall::op:deliver
        unmarshall::op:abort
        unmarshall::op:listen
        unmarshall::desc:import-object
        unmarshall::desc:import-promise
        unmarshall::desc:export
        unmarshall::desc:answer))

;; Internal commands from the vat connector
(struct cmd-send-message (msg)
  #:transparent)
(struct cmd-send-listen (to-refr listener-refr wants-partial?)
  #:transparent)

;; utility for splitting up keyword argument hashtable in a way usable by
;; keyword-apply
(define (kws-hasheq->kws-lists kw-args)
  (for/fold ([kws '()]
             [kw-vals '()])
            ([(key val) kw-args])
    (values (cons key kws)
            (cons val kw-vals))))

;; and the reverse
(define (kws-lists->kws-hasheq kws kw-vals)
  (for/fold ([ht #hasheq()])
            ([kw (in-list kws)]
             [kw-val (in-list kw-vals)])
    (hash-set ht kw kw-val)))


;; TODO: use me when we add gc support!
#;(struct export (refr count))
;; (struct question (refr [resolution #:mutable]))

#;(struct answer (local-promise [resolution #:mutable]))

(struct resolved-val (val))
(struct resolved-err (err))


;; TODO: This is really mixing up both captp and vattp into one thing.
;;   Kind of a mess... we should separate them.
;;   The biggest challenge is how to handle the (un)marshalling correctly.
;; TODO: Couldn't this just be an actor that's drip-fed messages
;;   pulled off the wire, running in its own vat?
(define (make-captp-thread captp-outgoing-ch from-machine-representative-ch
                           machine-vat-connector
                           bootstrap-refr)
  (define captp-incoming-ch
    (make-async-channel))
  (define internal-ch
    (make-async-channel))

  ;; position sealers, so we know this really is from our imports/exports
  ;; @@: Not great protection, subject to a reuse attack, but really
  ;;   this is just an extra step... in general we shouldn't be exposing
  ;;   the refr internals to most users
  (define-values (pos-seal pos-unseal pos-sealed?)
    (make-sealer-triplet))
  (define-values (partition-seal partition-unseal partition-tm?)
    (make-sealer-triplet))

  ;; Question finders are a weird thing... we need some way to be able to
  ;; look up what question corresponds to an entry in the table.
  ;; Used by mactor:question (a special kind of promise),
  ;; since messages sent to a question are pipelined through the answer
  ;; side of some "remote" machine.
  (struct question-finder ())

  ;; TODO: this is borrowed from vat.rkt, we should probably just make
  ;;   a generalized version of it.
  ;;   Definitely overkill for how used so far though...
  (define-syntax-rule (define-captp-dispatcher id [method-name method-handler] ...)
    (define id
      (procedure-rename
       (make-keyword-procedure
        (λ (kws kw-vals this-method-name . args)
          (define method
            (case this-method-name
              ['method-name method-handler] ...
              [else (error 'connector-dispatcher-error
                           "Unnown method: ~a" this-method-name)]))
          (keyword-apply method kws kw-vals args)))
       'id)))

  (define (_handle-message msg)
    (match msg
      [(? message?)
       (async-channel-put internal-ch
                          (cmd-send-message msg))]
      [(listen-request to-refr listener wants-partial?)
       (async-channel-put internal-ch
                          (cmd-send-listen to-refr listener
                                           wants-partial?))])
    (void))

  (define (_new-question-finder)
    (question-finder))

  (define (_partition-unsealer-tm-cons)
    (cons partition-unseal partition-tm?))

  (define (_listen-request to-refr listen-refr
                           #:wants-partial? [wants-partial? #f])
    (async-channel-put internal-ch
                       (cmd-send-listen to-refr listen-refr
                                        wants-partial?)))

  (define-captp-dispatcher captp-connector
    [handle-message _handle-message]
    [new-question-finder _new-question-finder]
    [listen _listen-request]
    [partition-unsealer-tm-cons _partition-unsealer-tm-cons])

  (syscaller-free-thread
   (lambda ()
     (define next-export-pos 0)
     (define next-question-pos 0)
     ;; (define next-promise-pos 0)

     ;; TODO: We need to wrap this in a structure that keeps track of when it
     ;;   can GC
     (define exports-val2pos (make-weak-hasheq))  ; exports[val]:   chosen by us
     (define exports-pos2val (make-hasheqv))      ; exports[pos]:  chosen by us
     ;; TODO: This doesn't make sense if the value isn't wrapped in a weak
     ;;   reference... I think this also needs to go in both directions to work
     ;;   from a GC perspective
     (define imports (make-hasheqv))               ; imports:        chosen by peer
     (define questions (make-hasheqv))             ; questions:      chosen by us
     (define answers (make-hasheqv))               ; answers:        chosen by peer

     ;; TODO: This should really be some kind of box that the other side
     ;;   can query, right?
     (define running? #t)

     ;; Possibly install an export for this local refr, and return
     ;; this export id
     ;; TODO: we maybe need to differentiate between local-live-refr and
     ;;   remote-live-proxy-refr (once we set that up)?
     (define/contract (maybe-install-export! refr)
       (-> live-refr? any/c)  ; TODO: Maybe de-contract this and manually check for speed
       (cond
         ;; Already have it, no need to increment next-export-pos
         [(hash-has-key? exports-val2pos refr)
          (hash-ref exports-val2pos refr)]
         ;; Nope, let's export this
         [else
          ;; TODO: This doesn't do handoffs for remote refrs yet!!
          ;; get this export-pos and increment next-export-pos
          (define export-pos
            next-export-pos)
          (set! next-export-pos (add1 export-pos))
          ;; install in both export tables
          (hash-set! exports-pos2val export-pos
                     refr)
          (hash-set! exports-val2pos refr
                     export-pos)
          export-pos]))

     (define/contract (marshall-local-refr! local-refr)
       (-> local-refr? (or/c desc:import-object
                             desc:import-promise))
       (define export-pos
         (maybe-install-export! local-refr))
       (match local-refr
         [(? local-object?)
          (desc:import-object export-pos)]
         [(? local-promise?)
          (desc:import-promise export-pos)]))

     (define (maybe-install-import! import-desc)
       (define import-pos
         (desc:import-pos import-desc))
       (cond
         [(hash-has-key? imports import-pos)
          ;; Oh, we've already got that.  Reference and return it.
          (hash-ref imports import-pos)]
         [else
          ;; construct the new reference...
          (define new-refr
            (match import-desc
              [(? desc:import-object?)
               (make-remote-object-refr captp-connector
                                        (pos-seal import-pos))]
              [(? desc:import-promise?)
               (make-remote-promise-refr captp-connector
                                         (pos-seal import-pos))]))
          ;; TODO: weak boxing goes here
          ;; install it...
          (hash-set! imports import-pos new-refr)
          ;; and return it.
          new-refr]))

     (define (remote-refr->imported-pos to)
       (pos-unseal (remote-refr-sealed-pos to)))

     (define (question-finder->question-pos! question-finder)
       (if (hash-has-key? questions question-finder)
           ;; we already have a question relevant to this question id
           (hash-ref questions question-finder)
           ;; new question id...
           (let ([question-pos next-question-pos])
             ;; install our question at this question id
             (hash-set! questions question-finder question-pos)
             ;; increment the next-question id
             (set! next-question-pos (add1 next-question-pos))
             ;; and return the question-pos we set up
             question-pos)))

     ;; general argument marshall/unmarshall for import/export

     ;; TODO: need to handle lists/dotted-lists/vectors
     (define (outgoing-pre-marshall! obj)
       (match obj
         [(? list?)
          (map outgoing-pre-marshall! obj)]
         [(? hash?)
          (for/fold ([ht #hash()])
                    ([(key val) obj])
            (hash-set ht (outgoing-pre-marshall! key)
                      (outgoing-pre-marshall! val)))]
         [(? set?)
          (for/set ([x obj])
            (outgoing-pre-marshall! x))]
         [(? local-promise?)
          (desc:import-promise (maybe-install-export! obj))]
         [(? local-object?)
          (desc:import-object (maybe-install-export! obj))]
         [(? remote-refr?)
          (define refr-captp-connector
            (remote-refr-captp-connector obj))
          (cond
            ;; from this captp
            [(eq? refr-captp-connector captp-connector)
             (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
            [else
             (error 'handoffs-not-supported-yet)])]
         ;; TODO: Supply more machine-crossing exception types here
         [(? exn:fail?)
          (record* 'exn:fail:mystery)]
         [_ obj]))

     (define (incoming-post-unmarshall! obj)
       (match obj
         [(? list?)
          (map incoming-post-unmarshall! obj)]
         [(? hash?)
          (for/fold ([ht #hash()])
                    ([(key val) obj])
            (hash-set ht (incoming-post-unmarshall! key)
                      (incoming-post-unmarshall! val)))]
         [(? set?)
          (for/set ([x obj])
            (incoming-post-unmarshall! x))]
         [(or (? desc:import-promise?) (? desc:import-object?))
          (maybe-install-import! obj)]
         [(desc:export pos)
          (hash-ref exports-pos2val pos)]
         [(record 'exn:fail:mystery '())
          (make-mystery-fail)]
         [_ obj]))

     (define (unmarshall-to-desc to-desc)
       (match to-desc
         [(desc:export export-pos)
          (hash-ref exports-pos2val export-pos)]
         [(desc:answer answer-pos)
          (hash-ref answers answer-pos)]))

     (define (marshall-to obj)
       (match obj
         [(? question-finder?)
          (desc:answer (hash-ref questions obj))]
         [(? remote-refr?)
          (define refr-captp-connector
            (remote-refr-captp-connector obj))
          (cond
            ;; from this captp
            [(eq? refr-captp-connector captp-connector)
             (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
            [else
             (error 'captp-to-wrong-machine)])]))

     (define (install-answer! answer-pos resolve-me-desc)
       (define resolve-me
         (maybe-install-import! resolve-me-desc))
       (when (hash-has-key? answers answer-pos)
         (error 'already-have-answer
                "~a" answer-pos))
       (match-define (cons answer-promise answer-resolver)
         (machine-vat-connector 'run spawn-promise-cons))
       (hash-set! answers answer-pos
                  answer-promise)
       (machine-vat-connector
        'run
        (lambda ()
          (listen answer-promise resolve-me)))
       (values answer-promise answer-resolver))

     (define (send-to-remote msg)
       (async-channel-put captp-outgoing-ch msg))

     (call/ec
      (lambda (escape-lp)
        ;; TODO: Also inform the parent machine that we are dead
        (define (tear-it-down)
          (set! exports-val2pos #f)
          (set! exports-pos2val #f)
          (set! imports #f)
          (set! questions #f)
          (set! answers #f)
          (set! running? #f)
          (escape-lp))

        (define (abort-because reason)
          (send-to-remote (op:abort reason))
          (tear-it-down))

        (define (handle-captp-incoming msg)
          (match msg
            [(op:bootstrap (? integer? answer-pos) resolve-me-desc)
             (define-values (answer-promise answer-resolver)
               (install-answer! answer-pos resolve-me-desc))

             ;; And since we're bootstrapping, we resolve it immediately
             (machine-vat-connector
              'call answer-resolver 'fulfill bootstrap-refr)
             (void)]
            ;; TODO: Handle case where the target doesn't exist?
            ;;   Or maybe just generally handle unmarshalling errors :P
            [(op:deliver-only to-desc method
                              args-marshalled
                              kw-args-marshalled)
             ;; TODO: support distinction between method sends and procedure sends
             (define args
               (incoming-post-unmarshall! args-marshalled))
             (define kw-args
               (incoming-post-unmarshall! kw-args-marshalled))
             (define target (unmarshall-to-desc to-desc))
             (define-values (kws kw-vals)
               (kws-hasheq->kws-lists kw-args))
             (keyword-apply machine-vat-connector
                            kws kw-vals
                            '<-np target
                            args)]
            [(op:deliver to-desc method
                         args-marshalled
                         kw-args-marshalled
                         answer-pos
                         resolve-me-desc)
             (define-values (answer-promise answer-resolver)
               (install-answer! answer-pos resolve-me-desc))

             ;; TODO: support distinction between method sends and procedure sends
             (define args
               (incoming-post-unmarshall! args-marshalled))
             (define kw-args
               (incoming-post-unmarshall! kw-args-marshalled))
             (define target (unmarshall-to-desc to-desc))
             (define-values (kws kw-vals)
               (kws-hasheq->kws-lists kw-args))
             (define sent-promise
               (machine-vat-connector
                'run
                (lambda ()
                  (keyword-apply <- kws kw-vals target args))))
             (machine-vat-connector
              'call answer-resolver 'fulfill sent-promise)]

            [(op:listen (? desc:export? to-desc)
                        (? desc:import? listener-desc)
                        (? boolean? wants-partial?))
             (define to-refr
               (unmarshall-to-desc to-desc))
             (define listener
               (incoming-post-unmarshall! listener-desc))
             (machine-vat-connector
              'run
              (λ ()
                (listen to-refr listener
                        #:wants-partial? wants-partial?)
                (void)))]
            [(op:abort reason)
             'TODO]
            
            [other-message
             (error 'invalid-message "~a" other-message)]))

        (define (handle-internal cmd)
          (match cmd
            [(cmd-send-message msg)
             (match-define (message to resolve-me kws kw-vals args)
               msg)
             (define answer-pos
               (if (question-message? msg)
                   (question-finder->question-pos!
                    (question-message-answer-this-question msg))
                   #f))
             (define deliver-msg
               (if resolve-me
                   (op:deliver (marshall-to to)
                               #;(desc:import (maybe-install-export! to))
                               #f ;; TODO: support methods
                               ;; TODO: correctly marshall everything here
                               (outgoing-pre-marshall! args)
                               (outgoing-pre-marshall!
                                (kws-lists->kws-hasheq kws kw-vals))
                               answer-pos
                               (marshall-local-refr! resolve-me))
                   (op:deliver-only (marshall-to to)
                                    #f ;; TODO: support methods
                                    (outgoing-pre-marshall! args)
                                    (outgoing-pre-marshall!
                                     (kws-lists->kws-hasheq kws kw-vals)))))
             (send-to-remote deliver-msg)]
            [(cmd-send-listen (? remote-refr? to-refr) (? local-refr? listener-refr)
                              (? boolean? wants-partial?))
             (define listen-msg
               (op:listen (marshall-to to-refr)
                          (outgoing-pre-marshall! listener-refr)
                          wants-partial?))
             (send-to-remote listen-msg)]))

        (define (handle-from-machine-representative msg)
          (match msg
            [(vector 'get-bootstrap-promise return-ch)
             (channel-put return-ch bootstrap-promise)]))

        ;;; BEGIN REMOTE BOOTSTRAP OPERATION
        ;;; ================================
        (define (bootstrap-remote!)
          (machine-vat-connector
           'run
           (lambda ()
             (define this-question-finder
               (question-finder))
             ;; called for its effect of installing the question
             (question-finder->question-pos! this-question-finder)
             (define-values (bootstrap-promise bootstrap-resolver)
               (_spawn-promise-values #:question-finder
                                      this-question-finder
                                      #:captp-connector
                                      captp-connector))
             (define bootstrap-msg
               (op:bootstrap (hash-ref questions this-question-finder)
                             (outgoing-pre-marshall! bootstrap-resolver)))
             (send-to-remote bootstrap-msg)
             bootstrap-promise)))
        (define bootstrap-promise
          (bootstrap-remote!))
        ;;; END REMOTE BOOTSTRAP OPERATION
        ;;; ==============================

        (let lp ()
          (sync (choice-evt (handle-evt captp-incoming-ch
                                        handle-captp-incoming)
                            (handle-evt internal-ch
                                        handle-internal)
                            (handle-evt from-machine-representative-ch
                                        handle-from-machine-representative)))
          (lp))))))
  captp-incoming-ch)

(define (make-machinetp-thread network-in-port network-out-port
                               machine-vat-connector
                               bootstrap-refr)
  (define from-machine-representative-ch
    (make-async-channel))
  (define captp-outgoing-ch
    (make-async-channel))
  (define captp-incoming-ch
    (make-captp-thread captp-outgoing-ch from-machine-representative-ch
                       machine-vat-connector
                       bootstrap-refr))

  ;; Now spawn threads that read/write to these ports
  (syscaller-free-thread
   (lambda ()
     (let lp ()
       (define msg
         ;; TODO: The marshalling/unmarshalling will have to move to
         ;;   another step inside the captp stuff rather than just on
         ;;   datastructure parsing to work right once we introduce
         ;;   more advanced input/output stuff...?
         ;;   Or at least, this will merely represent the "naive"
         ;;   structure from the wire.
         ;;   We will need another "reification" step in addition
         ;;   to this.
         (syrup-read network-in-port #:unmarshallers unmarshallers))
       (async-channel-put captp-incoming-ch msg)
       (lp))))

  (syscaller-free-thread
   (lambda ()
     (let lp ()
       (define msg
         (async-channel-get captp-outgoing-ch))
       (syrup-write msg network-out-port #:marshallers marshallers)
       (lp))))
  from-machine-representative-ch)

;; TODO: Need to think this through more.
;; Things machines need:
;;  - a way to start connections to (known) external machines
;;  - something something drivers?  (Nah can come later)
(define (spawn-machine-representative external-vat-send)
  (define (^machine-representative bcom)
    

    'TODO)
  (define machine-representative
    (spawn ^machine-representative))
  machine-representative)

(define (make-swiss-num)
  (crypto-random-bytes 32))

(define (^nonce-registry bcom)
  (let next-self ([ht #hash()])
    ; TODO: Maybe de-contract this and manually check for speed
    (define/contract (register refr)
      (-> live-refr? any/c)
      (define swiss-num
        (make-swiss-num))
      (define new-ht
        (hash-set ht swiss-num refr))
      (bcom (next-self new-ht)
            swiss-num))
    (methods
     [register register]
     [(fetch swiss-num)
      ;; TODO: Better errors when no swiss num
      (hash-ref ht swiss-num)])))

(define (spawn-nonce-registry-locator-pair)
  (define registry
    (spawn ^nonce-registry))
  (define (^nonce-locator bcom)
    (methods
     [(fetch swiss-num)
      ($ registry 'fetch swiss-num)]))
  (define locator
    (spawn ^nonce-locator))
  (cons registry locator))

;; TODO: these aren't really tests yet, just a working area...
(module+ test
  (require rackunit)

  ;; Testing against a machine representative with nothing real on the other side.
  ;; We're calling our end the "repl" end even if inaccurate ;)
  (define-values (repl->test1-ip repl->test1-op)
    (make-pipe))
  (define-values (test1->repl-ip test1->repl-op)
    (make-pipe))
  (define test1-vat
    (make-vat))

  (define (spawn-extended-registry-locator-pair)
    (match-define (cons registry locator)
      (spawn-nonce-registry-locator-pair))
    (define (^extended-locator bcom)
      (methods
       #:extends locator
       [(respond-to name)
        (channel-put test1-bootstrap-response-ch
                     `(hello ,name))]
       [(break-me)
        (error 'ahhh-i-am-broken)]))
    (define extended-locator
      (spawn ^extended-locator))
    (cons registry extended-locator))

  (match-define (cons test1-registry test1-locator)
    (test1-vat 'run spawn-extended-registry-locator-pair))

  ;; We'll use this to see if the bootstrap object ever gets our message
  (define test1-bootstrap-response-ch
    (make-channel))

  ;; We add an extra method here just for testing for basic communication
  (define test1-bootstrap-actor
    (test1-vat 'spawn
               (lambda (bcom)
                 (methods
                  #:extends test1-locator
                  [(respond-to name)
                   (channel-put test1-bootstrap-response-ch
                                `(hello ,name))]
                  [(break-me)
                   (error 'ahhh-i-am-broken)]))))

  (define repl-mach->test1-thread-ch
    (make-machinetp-thread repl->test1-ip test1->repl-op
                           test1-vat
                           test1-bootstrap-actor))

  ;; It should send us (the REPL) a bootstrap message with answer pos 0
  (test-equal?
   "test machine should send a bootstrap message"
   (syrup-read test1->repl-ip
               #:unmarshallers unmarshallers)
   (op:bootstrap 0 (desc:import-object 0)))

  ;; Now we should bootstrap it such that it allocates an answer for us
  (let ()
    (syrup-write (op:bootstrap 0 (desc:import-object 0))
                 repl->test1-op
                 #:marshallers marshallers)
    (void))

  ;; Now we should be able to submit a test message
  (let ()
    (syrup-write (op:deliver-only (desc:answer 0) #f '(respond-to REPL-friend) #hasheq())
                 repl->test1-op
                 #:marshallers marshallers)
    (void))

  (test-equal?
   "bootstrap actor is able to get messages from us"
   (sync/timeout
    0.5
    test1-bootstrap-response-ch)
   '(hello REPL-friend))

  ;; Now we want to experiment with using the bootstrap actor like it was a
  ;; normal bootstrap actor... ie, for (proto-)sturdyref lookup across the wire.
  (define ((^greeter bcom my-name) your-name)
    (format "<~a> Hello ~a!" my-name your-name))
  (define terrance
    (test1-vat 'spawn ^greeter "Terrance"))
  (define trisha
    (test1-vat 'spawn ^greeter "trisha"))

  (define terrance-nonce
    (test1-vat 'call test1-registry 'register terrance))
  (define trisha-nonce
    (test1-vat 'call test1-registry 'register trisha))

  ;; Now, let's just make sure this registry / thing works


  ;; Vat A -> Vat B tests
  ;; (TODO: Replace this with machines in separate places)
  (define a-vat
    (make-vat))
  (define b-vat
    (make-vat))

  (define-values (a->b-ip a->b-op)
    (make-pipe))
  (define-values (b->a-ip b->a-op)
    (make-pipe))

  (match-define (cons a-nonce-reg a-nonce-loc)
    (a-vat 'run spawn-extended-registry-locator-pair))
  (match-define (cons b-nonce-reg b-nonce-loc)
    (b-vat 'run spawn-extended-registry-locator-pair))

  (define a-machinetp-ch
    (make-machinetp-thread b->a-ip a->b-op
                           a-vat
                           a-nonce-loc))

  (define b-machinetp-ch
    (make-machinetp-thread a->b-ip b->a-op
                           b-vat
                           b-nonce-loc))

  (define a->b-bootstrap-vow
    (let ([return-ch (make-channel)])
      (async-channel-put a-machinetp-ch
                         (vector 'get-bootstrap-promise return-ch))
      (channel-get return-ch)))

  (define b->a-bootstrap-vow
    (let ([return-ch (make-channel)])
      (async-channel-put b-machinetp-ch
                         (vector 'get-bootstrap-promise return-ch))
      (channel-get return-ch)))

  ;; Now let's spawn bob in vat A
  (define bob-greeter1
    (b-vat 'spawn ^greeter "Bob"))
  ;; And get a nonce from A's registry
  (define bob-greeter1-nonce
    (b-vat 'call b-nonce-reg 'register bob-greeter1))
  (define bob-greeter1-sez-ch
    (make-async-channel))
  (a-vat 'run
         (lambda ()
           (on (<- a->b-bootstrap-vow 'fetch bob-greeter1-nonce)
               (lambda (bob)
                 (on (<- bob "Alyssa")
                     (lambda (bob-sez)
                       (async-channel-put bob-greeter1-sez-ch
                                          `(fulfilled ,bob-sez))))))))
  (test-equal?
   "Non-pipelined sends to bob work"
   (sync/timeout 0.2 bob-greeter1-sez-ch)
   '(fulfilled "<Bob> Hello Alyssa!"))

  (define bob-greeter2
    (b-vat 'spawn ^greeter "Bob"))
  ;; And get a nonce from A's registry
  (define bob-greeter2-nonce
    (b-vat 'call b-nonce-reg 'register bob-greeter2))
  (define bob-greeter2-sez-ch
    (make-async-channel))
  (a-vat 'run
         (lambda ()
           (on (<- (<- a->b-bootstrap-vow 'fetch bob-greeter2-nonce)
                   "Alyssa")
               (lambda (bob-sez)
                 (async-channel-put bob-greeter2-sez-ch
                                    `(fulfilled ,bob-sez))))))
  (test-equal?
   "Pipelined sends to bob work"
   (sync/timeout 0.2 bob-greeter2-sez-ch)
   '(fulfilled "<Bob> Hello Alyssa!"))

  (define ((^broken-greeter bcom my-name) your-name)
    (error 'oh-no-i-broke "My name: ~a" my-name)
    (format "<~a> Hello ~a!" my-name your-name))

  (define broken-bob-greeter
    (b-vat 'spawn ^broken-greeter "Broken Bob"))
  (define broken-bob-greeter-nonce
    (b-vat 'call b-nonce-reg 'register broken-bob-greeter))
  (define broken-bob-greeter-ch
    (make-async-channel))
  (a-vat 'run
         (lambda ()
           (on (<- (<- a->b-bootstrap-vow 'fetch broken-bob-greeter-nonce)
                   "Alyssa")
               (lambda (bob-sez)
                 (async-channel-put broken-bob-greeter-ch
                                    `(fulfilled ,bob-sez)))
               #:catch
               (lambda (err)
                 (async-channel-put broken-bob-greeter-ch
                                    `(broken ,err))))))
  
  (test-true
   "Breakage correctly propagates across captp"
   (match (sync/timeout 0.2 broken-bob-greeter-ch)
     [(list 'broken problem)
      #t]
     [something-else
      #f]))

  ;; For that matter, here's a simpler test
  (define a->b-bootstrap-break-me-ch
    (make-async-channel))
  (a-vat 'run
         (lambda ()
           (on (<- a->b-bootstrap-vow 'break-me)
               (lambda (val)
                 (async-channel-put a->b-bootstrap-break-me-ch
                                    `(fulfilled val)))
               #:catch
               (lambda (err)
                 (async-channel-put a->b-bootstrap-break-me-ch
                                    `(broken err))))))
  (test-true
   "Breakage correctly propagates across captp, simple example"
   (match (sync/timeout 0.2 a->b-bootstrap-break-me-ch)
     [(list 'broken problem)
      #t]
     [something-else
      #f]))


  ;; Now for testing three vats across two machines.
  ;; This one, functionally, is actually running through a-vat's
  ;; machinetp system
  (define c-vat
    (make-vat))

  (define meeter-bob-response-ch
    (make-async-channel))

  (define introducer-alice
    (a-vat 'spawn
           (lambda (bcom)
             (lambda (intro-bob intro-carol)
               (<-np intro-bob 'meet intro-carol)))))
  (define meeter-bob
    (b-vat 'spawn
           (lambda (bcom)
             (methods
              [(meet new-friend)
               (on (<- new-friend 'hi-new-friend)
                   (lambda (heard-back)
                     (async-channel-put meeter-bob-response-ch
                                        `(heard-back ,heard-back))))]))))
  (define chatty-carol
    (c-vat 'spawn
           (lambda (bcom)
             (methods
              [(hi-new-friend)
               'hello-back]))))
  (define meeter-bob-nonce
    (b-vat 'call b-nonce-reg 'register meeter-bob))

  (a-vat 'run
         (lambda ()
           (<-np introducer-alice
                 (<- a->b-bootstrap-vow 'fetch meeter-bob-nonce)
                 chatty-carol)))

  (test-equal?
   "A and B on one machine, C on another, with introductions"
   (sync/timeout 0.5 meeter-bob-response-ch)
   '(heard-back hello-back))


  ;; ;; WIP WIP WIP WIP WIP
  ;; (define ((^parrot bcom) . args)
  ;;   (pk 'bawwwwk args))
  ;; (define parrot (b-vat 'spawn ^parrot))

  ;; ;; TODO: This apparently will need to register itself with the base
  ;; ;;   ^machine...
  ;; (define machine-representative->machine-thread-ch
  ;;   (make-machinetp-thread b->a-ip a->b-op
  ;;                          b-vat
  ;;                          bob))

  ;; (syrup-write (op:deliver-only 0 #f '("George") #hasheq())
  ;;              b->a-op
  ;;              #:marshallers marshallers)


  )
