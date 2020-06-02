#lang racket/base

(require racket/async-channel
         racket/match
         racket/random
         racket/contract
         "core.rkt"
         "vat.rkt"
         "actor-lib/methods.rkt"
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
  (question-id)
  marshall::op:bootstrap unmarshall::op:bootstrap)

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-recordable-struct op:deliver-only
  (;; Position in the table for the target
   ;; (sender's imports, reciever's exports)
   target-pos
   ;; Either the method name, or #f if this is a procedure call
   method
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
   args
   kw-args)
  marshall::op:deliver-only unmarshall::op:deliver-only)

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-recordable-struct op:deliver
  (answer-pos
   redirector  ; a resolver...?
   target-pos
   method
   args
   kw-args)
  marshall::op:deliver unmarshall::op:deliver)

(define-recordable-struct op:abort
  (reason)
  marshall::op:abort unmarshall::op:abort)

;; Effectively splitting "return" into two distinct operations
(define-recordable-struct op:return-fulfill
  (answer-id val)
  marshall::op:return-fulfill unmarshall::op:return-fulfill)
(define-recordable-struct op:return-break
  (answer-id problem)
  marshall::op:return-break unmarshall::op:return-break)

(define-recordable-struct desc:new-far-desc
  (import-pos)
  marshall::desc:new-far-desc unmarshall::desc:new-far-desc)

(define-recordable-struct desc:new-remote-promise
  (import-pos
   redirector-pos) ;; ?
  marshall::desc:new-remote-promise unmarshall::desc:new-remote-promise)


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
        marshall::op:return-fulfill
        marshall::op:return-break
        marshall::desc:new-far-desc
        marshall::desc:new-remote-promise))

(define unmarshallers
  (list unmarshall::op:bootstrap
        unmarshall::op:deliver-only
        unmarshall::op:deliver
        unmarshall::op:abort
        unmarshall::op:return-fulfill
        unmarshall::op:return-break
        unmarshall::desc:new-far-desc
        unmarshall::desc:new-remote-promise))


(define (make-machinetp-thread network-in-port network-out-port
                               machine-vat-connector
                               bootstrap-refr)
  (define send-to-machinetp-channel
    (make-async-channel))
  (syscaller-free-thread
   (lambda ()
     (define last-export-id 0)
     (define last-question-id 0)
     (define last-promise-id 0)

     ;; TODO: We need to wrap this in a structure that keeps track of when it
     ;;   can GC
     (define exports-val2slot (make-weak-hasheq))  ; exports[val]:   chosen by us
     (define exports-slot2val (make-hasheqv))      ; exports[slot]:  chosen by us
     ;; TODO: This doesn't make sense if the value isn't wrapped in a weak
     ;;   reference... I think this also needs to go in both directions to work
     ;;   from a GC perspective
     (define imports (make-hasheqv))               ; imports:        chosen by peer
     ;; (define questions (make-hasheqv))             ; questions:      chosen by us
     ;; (define answers (make-hasheqv))               ; answers:        chosen by peer

     ;; TODO: This should really be some kind of box that the other side
     ;;   can query, right?
     (define running? #t)

     (define (next-message)
       (syrup-read network-in-port #:unmarshallers unmarshallers))

     (define/contract (maybe-install-export! refr)
       (-> live-refr? any/c)  ; TODO: Maybe de-contract this and manually check for speed
       (cond
         ;; Already have it, no need to increment last-export-id
         [(hash-has-key? exports-val2slot refr)
          (hash-ref exports-val2slot refr)]
         ;; Nope, let's export this
         [else
          (define next-export-id
            (add1 last-export-id))
          ;; install in both export tables
          (hash-set! exports-slot2val next-export-id
                     refr)
          (hash-set! exports-val2slot refr
                     next-export-id)
          ;; increment last-export-id
          (set! last-export-id next-export-id)
          next-export-id]))

     ;; Install bootstrap object
     (when bootstrap-refr
       (hash-set! exports-slot2val bootstrap-refr 0)
       (hash-set! exports-val2slot 0 bootstrap-refr))

     (call/ec
      (lambda (escape-lp)
        ;; TODO: Also inform the parent machine that we are dead
        (define (tear-it-down)
          (set! exports-val2slot #f)
          (set! exports-slot2val #f)
          (set! imports #f)
          (set! running? #f)
          (escape-lp))
        (let lp ()
          (match (pk 'next-message (next-message))
            ;; For now we'll just assume a bootstrap object.
            #;[(op:bootstrap question-id)
             (pk 'bootstrapped)
             'TODO]
            [(op:deliver-only target-pos method args kw-args)
             (pk 'deliver-onlyed)
             ;; TODO: Handle case where the target doesn't exist
             (define target
               (hash-ref exports-val2slot target-pos))
             (define-values (kws kw-vals)
               (for/fold ([kws '()]
                          [kw-vals '()])
                         ([(key val) kw-args])
                 (values (cons key kws)
                         (cons val kw-vals))))
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
            [(op:deliver answer-pos redirector target-pos method args kw-args)
             (pk 'delivered)
             'TODO]
            [(op:abort reason)
             (pk 'aborted)
             'TODO]
            [other-message
             (pk 'unknown-message-type other-message)])
          (lp))))))
  send-to-machinetp-channel)

;; TODO: Need to think this through more.
;; Things machines need:
;;  - a way to start connections to (known) external machines
;;  - something something drivers?  (Nah can come later)
(define (spawn-machine external-vat-send)
  (define (^machine bcom)
    

    'TODO)
  (define machine
    (spawn ^machine))
  machine)


#;(define (^craptp-conn _bcom in-port out-port
                      
                      self-send-np)
  

  ;; TODO: Two faces to this:
  ;;  - a way for the thread to communicate with an actor
  ;;  - a way for those 

  (define (^for-comm-thread bcom)
    (methods
     [(something) 'TODO]))
  (define (^for-machine bcom)
    #;(methods
     [])
    ;; For now we'll just pass through to the thread.  This is reasonably unsafe
    ;; but we're testing things out.

    )

  (define thread-communicator
    (spawn ^for-comm-thread))

  (define machine-communicator
    (spawn ^for-machine))
  machine-communicator)

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

(module+ test
  (require rackunit)
  (define a-vat
    (make-vat))
  (define b-vat
    (make-vat))

  (define-values (a->b-ip a->b-op)
    (make-pipe))
  (define-values (b->a-ip b->a-op)
    (make-pipe))

  (match-define (cons a-nonce-loc a-nonce-reg)
    (a-vat 'run spawn-nonce-registry-locator-pair))
  (match-define (cons b-nonce-loc b-nonce-reg)
    (a-vat 'run spawn-nonce-registry-locator-pair))

  (define ((^greeter bcom my-name) your-name)
    (displayln (format "<~a> Hello ~a!" my-name your-name)))
  (define alice
    (a-vat 'spawn ^greeter "Alice"))

  ;; WIP WIP WIP WIP WIP
  (define ((^parrot bcom) . args)
    (pk 'bawwwwk args))
  (define parrot (a-vat 'spawn ^parrot))

  (define a-machinetp-thread-ch
    (make-machinetp-thread b->a-ip a->b-op
                           a-vat
                           alice))


  )
