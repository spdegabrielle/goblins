#lang racket

(require racket/exn
         racket/generic
         racket/promise
         racket/random
         racket/async-channel)

(module+ test
  (require rackunit))

(struct message
  (;; a possible randomly generated identifier for this message.
   ;; Only used across vats?
   ;; @@: Do we even need this?  Maybe the vat is responsible for handling
   ;;   a mapping of these when they move across boundaries?  Hm... nah...
   ;;   that would be expensive because figuring out when to GC ids
   ;;   would be hard.
   id
   ;; Address we're sending this to
   to
   ;; The arguments to the procedure we'll be calling
   body
   ;; If we're responding to another message
   in-reply-to
   ;; A request to reply to this actor address
   please-reply-to))

(define-generics address
  [address-id address])

;; TODO: This needs to be callable...
;; TODO: Maybe we merge these with remote-address
;; TODO: This needs a will for when there are no more references
;;   to it where we ask the vat to shut us down
;;   ... or should we use a custodian, actually?
(struct local-address
  (id vat-channel)
  #:methods gen:address
  [(define (address-id address)
     (force (local-address-id address)))])

(struct remote-address
  (id vat-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

(define (send-message to body
                      #:please-reply? [please-reply? #f]
                      #:in-reply-to [in-reply-to #f]
                      )
  "Send a message to TO address (through the vat), with BODY.
If PLEASE-REPLY? is true, ask for the recipient to eventually respond
to us."
  (define msg
    (message #f  ;; I guess we'll set! these later if need be?
             to body
             in-reply-to
             (if please-reply?
                 (self) #f)))
  (channel-put (current-vat-channel) (vector 'send-message msg))
  msg)

(define (<- to . body)
  (send-message to body)
  (void))

(define (<-wait to . body)
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation actor-prompt-tag k to body))
   actor-prompt-tag))

(define (<-block to . body)
  'TODO)

(provide <- <-wait <-block)

(define actor-prompt-tag
  (make-continuation-prompt-tag))

(define current-vat-channel
  (make-parameter #f))

;; Or actor-address?  Anyway maybe this should be a weak box?
(define self
  (make-parameter #f))

(provide current-vat-channel self)

(struct handle-message
  [msg actor-address])

(define (start-message-loop handler)
  "The actor main loop"
  (thread
   (lambda ()
     ;; This is a mapping of message => continuation where messages are
     ;; messages that are "waiting on a reply"
     ;; The Vat takes care of a (weak) mapping of random ids to messages
     ;; so we don't have to here.
     (define waiting-on-messages
       (make-hasheq))
     (define (with-message-capture-prompt thunk)
       (call-with-continuation-prompt
        thunk
        actor-prompt-tag
        (lambda (k to body)
          (define msg
            (send-message to body))
          ;; Set waiting-on-messages continuation to be this msg
          (hash-set! waiting-on-messages msg k))))
     (let lp ()
       (match (thread-receive)
         [(handle-message msg actor-address)
          (cond
           ;; This message is in reply to another... so handle it!
           [(message-in-reply-to msg) =>
            (lambda (in-reply-to)
              (when (not (hash-has-key? waiting-on-messages in-reply-to))
                (error "Message in reply to object that doesn't exist"
                       msg in-reply-to))
              ;; Pull the waiting continuation out
              (define waiting-kont (hash-ref waiting-on-messages in-reply-to))
              (hash-remove! waiting-on-messages in-reply-to)
              ;; Let's call it...
              (with-message-capture-prompt
               (lambda ()
                 (apply waiting-kont (message-body msg))))
              ;; Loop again
              (lp))]
           [else
            ;; Set up initial escape and capture prompts, along with error handler
            (with-message-capture-prompt
             (lambda ()
               ;; Why put self parameterization here?  Why not at the top?
               ;; If we put it here, we can ensure that while processing a message
               ;; that we keep self from being gc'ed, but we allow the possibility
               ;; of the message handler being GC'ed from the main root, allowing
               ;; for shutdown when no references are left
               (parameterize ([self actor-address])
                 (with-handlers ([exn:fail?
                                  (lambda (v)
                                    ;; Error handling goes here!
                                    (display (exn->string v)))])
                   (call-with-values
                       (lambda ()
                         (apply handler (message-body msg)))
                     (lambda vals
                       (when (message-please-reply-to msg)
                         (send-message (message-please-reply-to msg)
                                       vals
                                       #:in-reply-to msg))))))))
            (lp)])]
         ;; Got the shut down command...
         ['shutdown (void)])))))

;;; Check if the basic message loop stuff works
(module+ test
  (define set-this-box
    (box #f))
  (define wait-on
    (make-semaphore))
  (define actor-a
    (start-message-loop
     (lambda (wait-on foo)
       (set-box! set-this-box foo)
       (semaphore-post wait-on))
     ;; shouldn't come up but I guess we'll hand over a channel anyhow
     (make-channel)))
  (thread-send actor-a
               (handle-message (message #f actor-a (list wait-on 'beep) #f #f)
                               actor-a))
  (sync/timeout 1 wait-on)
  (check-eq? (unbox set-this-box) 'beep))


(struct registered-actor
  (thread custodian))

;; So we need flexible vats eventually.
;; But do we really need flexible actors? :\
(define vat%
  (class object%
    (super-new)
    (define actor-registry
      (make-weak-hasheq))
    (define vat-channel
      (make-channel))
    (define/public (get-vat-channel)
      vat-channel)

    (define vat-custodian
      (make-custodian))

    (define/public (main-loop)
      (parameterize ([current-custodian vat-custodian])
        (thread
         (lambda ()
           (let lp ()
             (match (channel-get vat-channel)
               ;; Send a message to an actor at a particular address
               [(vector 'send-message msg)
                (define msg-to
                  (message-to msg))
                ;; TODO: Remote vat support goes here
                (when (not (hash-has-key? actor-registry msg-to))
                  (error "No actor with id" msg-to))

                (define actor-reg
                  (hash-ref actor-registry msg-to))
                (thread-send (registered-actor-thread actor-reg)
                             (handle-message msg msg-to))
                (lp)]
               ;; Register an actor as part of this vat
               ;; TODO: Perhaps we should be the ones generating the address
               [(vector 'spawn-actor handler send-actor-address-ch)
                (define actor-custodian
                  (make-custodian))
                (define actor-thread
                  (parameterize ([current-custodian actor-custodian])
                    (start-message-loop
                     handler)))
                (define actor-address
                  (local-address #f vat-channel))
                (hash-set! actor-registry
                           actor-address (registered-actor actor-thread
                                                           actor-custodian))
                (channel-put send-actor-address-ch
                             actor-address)
                (lp)]
               ['shutdown
                (custodian-shutdown-all vat-custodian)
                (void)])))))
      (void))))

(provide vat%)

(define (spawn handler #:will [will #f])  ; TODO: Add will support
  (define (spawn-default-vat)
    (define new-vat
      (new vat%))
    (define vat-channel
      (send new-vat get-vat-channel))
    (send new-vat main-loop)
    (current-vat-channel vat-channel)
    vat-channel)
  (define vat-channel
    (or (current-vat-channel)
        (spawn-default-vat)))
  (define get-address-ch
    (make-channel))
  (channel-put vat-channel (vector 'spawn-actor handler get-address-ch))
  (define actor-address
    (channel-get get-address-ch))
  actor-address)





