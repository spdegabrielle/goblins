#lang racket/base

(require racket/async-channel
         racket/match
         racket/random
         racket/contract
         "core.rkt"
         (submod "core.rkt" for-captp)
         "message.rkt"
         "vat.rkt"
         "actor-lib/methods.rkt"
         "utils/simple-sealers.rkt"
         syrup)

(require pk)

;;; Messages

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
  (answer-id resolve-me)
  marshall::op:bootstrap unmarshall::op:bootstrap)

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-recordable-struct op:deliver-only
  (;; Position in the table for the target
   ;; (sender's imports, reciever's exports)
   target-desc
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
   resolve-me)  ; a resolver, probably an import (though it could be a handoff)
  marshall::op:deliver unmarshall::op:deliver)

(define-recordable-struct op:abort
  (reason)
  marshall::op:abort unmarshall::op:abort)

;; ;; Effectively splitting "return" into two distinct operations
;; (define-recordable-struct op:return-fulfill
;;   (answer-id val)
;;   marshall::op:return-fulfill unmarshall::op:return-fulfill)
;; (define-recordable-struct op:return-break
;;   (answer-id problem)
;;   marshall::op:return-break unmarshall::op:return-break)

;;; Descriptions of references sent across the wire
;;; (from receiver's perspective)

;; New import which we shouldn't have seen yet (if we have, it's an error)
;; TODO: I'm not sure if this actually is necessary or useful
#;(define-recordable-struct desc:new-import
  (import-pos)
  marshall::desc:new-import unmarshall::desc:new-import)

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

(define-recordable-struct desc:export-object
  (pos)
  marshall::desc:export-object unmarshall::desc:export-object)

(define-recordable-struct desc:export-promise
  (pos)
  marshall::desc:export-promise unmarshall::desc:export-promise)

(define (desc:export-pos export-desc)
  (match export-desc
    [(? desc:export-object?)
     (desc:export-object-pos export-desc)]
    [(? desc:export-promise?)
     (desc:export-promise-pos export-desc)]))


;; Something to answer that we haven't seen before.
;; As such, we need to set up both the promise import and this resolver/redirector
(define-recordable-struct desc:answer
  (pos)
  marshall::desc:answer unmarshall::desc:answer)

;; TODO: 3 vat/machine handoff versions (Promise3Desc, Far3Desc)


;; (define op:bootstrap->record
;;   (match-lambda
;;     [(op:bootstrap question-id)
;;      (record* 'op:bootstrap question-id)]))
;; (define op:deliver-only->record
;;   (match-lambda
;;     [(op:deliver-only target-pos method args kw-args)
;;      (record* 'op:deliver-only target-pos method args kw-args)]))

(define marshallers
  (list marshall::op:bootstrap
        marshall::op:deliver-only
        marshall::op:deliver
        marshall::op:abort
        ;; marshall::op:return-fulfill
        ;; marshall::op:return-break
        ;; marshall::desc:new-import
        marshall::desc:import-object
        marshall::desc:import-promise
        marshall::desc:answer))

(define unmarshallers
  (list unmarshall::op:bootstrap
        unmarshall::op:deliver-only
        unmarshall::op:deliver
        unmarshall::op:abort
        ;; unmarshall::op:return-fulfill
        ;; unmarshall::op:return-break
        ;; unmarshall::desc:new-import
        unmarshall::desc:import-object
        unmarshall::desc:import-promise
        unmarshall::desc:answer))

;; Internal commands from the vat connector
(struct cmd-deliver-message (msg question-promise))

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
            ([kw kws]
             [kw-val kw-vals])
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
(define (make-captp-thread captp-outgoing-ch from-machine-representative-ch
                           machine-vat-connector
                           bootstrap-refr)
  (define captp-incoming-ch
    (make-async-channel))
  (define internal-ch
    (make-async-channel))

  ;; position sealers, so we know this really is from our imports/exports
  (define-values (pos-seal pos-unseal pos-sealed?)
    (make-sealer-triplet))


  ;; TODO: this is borrowed from vat.rkt, we should probably just make
  ;;   a generalized version of it.
  ;;   Definitely overkill for how used so far though...
  (define-syntax-rule (define-captp-dispatcher id [method-name method-handler] ...)
    (define id
      (procedure-rename
       (make-keyword-procedure
        (λ (kws kw-args this-method-name . args)
          (define method
            (case this-method-name
              ['method-name method-handler] ...
              [else (error 'connector-dispatcher-error
                           "Unnown method: ~a" this-method-name)]))
          (keyword-apply method kws kw-args args)))
       'id)))

  (define (_deliver-message msg question-promise)
    (async-channel-put internal-ch
                       (cmd-deliver-message msg question-promise)))

  (define-captp-dispatcher captp-connector
    [deliver-message _deliver-message])

  (syscaller-free-thread
   (lambda ()
     (define next-export-id 0)
     (define next-question-id 0)
     ;; (define next-promise-id 0)

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
         ;; Already have it, no need to increment next-export-id
         [(hash-has-key? exports-val2pos refr)
          (hash-ref exports-val2pos refr)]
         ;; Nope, let's export this
         [else
          ;; TODO: This doesn't do handoffs for remote refrs yet!!
          ;; get this export-id and increment next-export-id
          (define export-id
            next-export-id)
          (set! next-export-id (add1 export-id))
          ;; install in both export tables
          (hash-set! exports-pos2val export-id
                     refr)
          (hash-set! exports-val2pos refr
                     export-id)
          export-id]))

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

     ;; Questions are kind of weird because:
     ;;   a) they only happen via captp and
     ;;   b) they have both a question and resolver pair
     (define (maybe-install-question! question-promise question-resolver)
       ;; get this question-id and increment next-question-id
       (define question-id
         next-question-id)
       (set! next-question-id (add1 next-question-id))

       (hash-set! questions question-id
                  (cons question-promise question-resolver))

       question-id)

     ;; Needs to return a desc
     (define (resolve-delivery-to! to)
       'TODO)

     ;; ;; Install bootstrap object
     ;; (when bootstrap-refr
     ;;   (hash-set! exports-pos2val bootstrap-refr 0)
     ;;   (hash-set! exports-val2pos 0 bootstrap-refr))

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
            [(op:bootstrap answer-id resolve-me)
             (pk 'bootstrapped)
             ;; TODO: Dramatically incorrect, but we'll fix in the next step.
             ;;   The values in the answers table shouldn't be just local references,
             ;;   because we usually won't know yet.  This is just an intermediately
             ;;   incorrect stepping stone...
             (hash-set! answers answer-id bootstrap-refr)]
            [(op:deliver-only target-desc method args kw-args)
             (pk 'deliver-onlyed)
             ;; TODO: Handle case where the target doesn't exist
             (define target
               (match target-desc
                 [(or (desc:import-object import-pos)
                      (desc:import-promise import-pos))
                  (hash-ref exports-pos2val import-pos)]
                 [(desc:answer answer-pos)
                  ;; TODO: Super, super wrong; this won't remain a direct value.
                  ;;    See op:bootstrap about what we need to change here...
                  (hash-ref answers answer-pos)]))
             (define-values (kws kw-vals)
               (kws-hasheq->kws-lists kw-args))
             ;; TODO: support distinction between method sends and procedure sends
             (if method
                 (keyword-apply machine-vat-connector
                                kws kw-vals
                                '<-np target
                                method args)
                 (keyword-apply machine-vat-connector
                                kws kw-vals
                                '<-np target
                                args))]
            ;;  Let's finish on this one once we get bootstrapping working
            #;[(op:deliver answer-pos redirector target-pos method args kw-args)
             (pk 'delivered)
             ;; TODO: Handle case where the target doesn't exist
             (define target
               (hash-ref exports-val2pos target-pos))
             (define-values (kws kw-vals)
               (kws-hasheq->kws-lists kw-args))
             ;; @@: Is this what goes into the answers table...?
             ;;   It doesn't completely seem that way because when 

             ;; TODO: support distinction between method sends and procedure sends
             (define local-promise
               (if method
                   (keyword-apply machine-vat-connector
                                  kws kw-vals
                                  '<- target
                                  method args)
                   (keyword-apply machine-vat-connector
                                  kws kw-vals
                                  '<- target
                                  args)))
             (machine-vat-connector
              'run
              (lambda ()
                (on local-promise
                    (lambda (val)
                      (async-channel-put captp-incoming-ch
                                         (internal-msg-seal
                                          `#(resolved-val ,answer-pos ,val))))
                    #:catch
                    (lambda (err)
                      (async-channel-put captp-incoming-ch
                                         (internal-msg-seal
                                          `#(resolved-err ,answer-pos ,err)))))))]
            [(op:abort reason)
             (pk 'aborted)
             'TODO]
            #;[(? internal-msg? internal-msg)
             (match internal-msg
               [(vector 'resolved-val answer-pos val)
                ;; ok what needs to happen here?
                ;;  - we need to resolve


                ]
               [(vector 'resolved-err answer-pos err)
                ]
               )
             'TODO]
            [other-message
             (pk 'unknown-message-type other-message)]))

        (define (handle-internal cmd)
          (match cmd
            [(cmd-deliver-message (message to resolve-me kws kw-vals args)
                                  question-promise)
             (define deliver-msg
               (op:deliver (resolve-delivery-to! to)
                           #;(desc:import (maybe-install-export! to))
                           #f ;; TODO: support methods
                           ;; TODO: correctly marshall everything here
                           args
                           (kws-lists->kws-hasheq kws kw-vals)
                           (maybe-install-question! resolve-me)))
             (send-to-remote (syrup-encode deliver-msg
                                           #:marshallers marshallers))
             
             'TODO]
            
            ))

        (define (handle-from-machine-representative msg)
          #;(match msg
            [(vector 'deliver-only remote-live-refr target-pos method args kw-args)
             (pk 'repr-deliver-only)
             'TODO]
            [(vector 'bootstrap-deliver-only method args kw-args)
             (pk 'repr-bootstrap-deliver-only)
             (send-to-remote (op:deliver-only (desc:answer bootstrap-question-id)
                                              method args kw-args))
             'TODO])
          'TODO)

        ;;; BEGIN REMOTE BOOTSTRAP OPERATION
        ;;; ================================
        ;; First we need a promise/resolver pair for the bootstrap
        ;; object (wait... is this true for this one though?  Don't we
        ;; just need the id?)
        (match-define (cons bootstrap-promise bootstrap-resolver)
          (machine-vat-connector
           'run (lambda ()
                  (define-values (promise resolver)
                    (_spawn-promise-values
                     #:question-captp-connector captp-connector))
                  (cons promise resolver))))
        ;; Now install this question
        (define bootstrap-question-id
          #;(install-question! bootstrap-promise bootstrap-resolver)
          ;; TODO: Install promise and resolver I guess?  Not sure
          (maybe-install-question! #f #f))
        ;; Now send the bootstrap message to the other side
        (send-to-remote (op:bootstrap bootstrap-question-id #f))
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

  (match-define (cons test1-registry test1-locator)
    (test1-vat 'run spawn-nonce-registry-locator-pair))

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
                                `(hello ,name))]))))

  (define repl-mach->test1-thread-ch
    (make-machinetp-thread repl->test1-ip test1->repl-op
                           test1-vat
                           test1-bootstrap-actor))

  ;; It should send us (the REPL) a bootstrap message with answer pos 0
  (test-equal?
   "test machine should send a bootstrap message"
   (syrup-read test1->repl-ip
               #:unmarshallers unmarshallers)
   (op:bootstrap 0 #f))

  ;; Now we should bootstrap it such that it allocates an answer for us
  (syrup-write (op:bootstrap 0 #f)
               repl->test1-op
               #:marshallers marshallers)

  ;; Now we should be able to submit a test message
  (syrup-write (op:deliver-only (desc:answer 0) #f '(respond-to REPL-friend) #hasheq())
               repl->test1-op
               #:marshallers marshallers)

  (test-equal?
   "bootstrap actor is able to get messages from us"
   (sync/timeout
    0.5
    test1-bootstrap-response-ch)
   '(hello REPL-friend))

  ;; Now we want to experiment with using the bootstrap actor like it was a
  ;; normal bootstrap actor... ie, for (proto-)sturdyref lookup across the wire.
  (define ((^greeter bcom my-name) your-name)
    (displayln (format "<~a> Hello ~a!" my-name your-name)))
  (define terrance
    (test1-vat 'spawn ^greeter "Terrance"))
  (define trisha
    (test1-vat 'spawn ^greeter "trisha"))

  (define terrance-nonce
    (test1-vat 'call test1-registry 'register terrance))
  (define trisha-nonce
    (test1-vat 'call test1-registry 'register trisha))

  ;; Now, let's just make sure this registry / thing works

  




  ;; ;; Vat A -> Vat B tests
  ;; ;; (TODO: Replace this with machines in separate places)
  ;; (define a-vat
  ;;   (make-vat))
  ;; (define b-vat
  ;;   (make-vat))

  ;; (define-values (a->b-ip a->b-op)
  ;;   (make-pipe))
  ;; (define-values (b->a-ip b->a-op)
  ;;   (make-pipe))

  ;; (match-define (cons a-nonce-loc a-nonce-reg)
  ;;   (a-vat 'run spawn-nonce-registry-locator-pair))
  ;; (match-define (cons b-nonce-loc b-nonce-reg)
  ;;   (a-vat 'run spawn-nonce-registry-locator-pair))

  ;; (define ((^greeter bcom my-name) your-name)
  ;;   (displayln (format "<~a> Hello ~a!" my-name your-name)))
  ;; (define alice
  ;;   (a-vat 'spawn ^greeter "Alice"))

  ;; ;; WIP WIP WIP WIP WIP
  ;; (define ((^parrot bcom) . args)
  ;;   (pk 'bawwwwk args))
  ;; (define parrot (a-vat 'spawn ^parrot))

  ;; ;; TODO: This apparently will need to register itself with the base
  ;; ;;   ^machine...
  ;; (define machine-representative->machine-thread-ch
  ;;   (make-machinetp-thread b->a-ip a->b-op
  ;;                          a-vat
  ;;                          alice))

  ;; (syrup-write (op:deliver-only 0 #f '("George") #hasheq())
  ;;              b->a-op
  ;;              #:marshallers marshallers)


  )
