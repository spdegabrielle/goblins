#lang racket

(require racket/generic
         racket/promise
         racket/random
         racket/async-channel)

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
  (channel-put (current-vat)
               msg)
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

(define actor-prompt-tag
  (make-continuation-prompt-tag))

(define current-vat
  (make-parameter #f))

;; Or actor-address?  Anyway maybe this should be a weak box?
(define self
  (make-parameter #f))

(struct handle-message
  [msg actor-address])

(define (start-message-loop handler vat-channel)
  "The actor main loop"
  (thread
   (lambda ()
     (parameterize ([current-vat vat-channel])
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
           ['shutdown (void)]))))))

;; So we need flexible vats.
;; But do we really need flexible actors? :\
(define vat%
  (class object%
    (super-new)
    (define actor-registry
      (make-weak-hasheq))))

(define-generics address
  [address-id address])

;; TODO: This needs to be callable...
;; TODO: Maybe we merge these with remote-address
(struct local-address
  (id vat)
  #:methods gen:address
  [(define (address-id address)
     (force (local-address-id address)))])

(define (fresh-local-address)
  (local-address #f))

(struct remote-address
  (id vat-ref)
  #:methods gen:address
  [(define (address-id address)
     (remote-address-id address))])

#;(define spawn
  (make-keyword-procedure
   (lambda (kws kw-args actor-constructor . rest)
     (thread
      ;; Not quite complete but getting there...
      (lambda ()
        (define actor-thread
          (thread
           (lambda ()
             (message-loop
              (keyword-apply actor-constructor kws kw-args rest)
              ))))
        ;; TODO: Register with vat

        ;; TODO: Start listening loop

        ;; TODO: Return actor address, which is callable for <-wait style behavior
        

        )))))





(provide self <-)
