#lang racket/base

(require "core.rkt"
         racket/async-channel
         racket/match

         "actor-lib/methods.rkt"

         crypto
         crypto/private/common/base256
         
         [only-in racket/promise delay delay/sync force]

         racket/exn
         racket/contract)

;;; Core commands
;;; 

(struct cmd-install-network (network))
;; Do we actually need to do this?
;; When does registering a vat ever need to occur tbh?
;; live-refrs have their own vat-connector
#;(struct cmd-register-local-vat (vat-conn))
(struct cmd-register-remote-promise (promise-foo))
(struct cmd-make-sturdyrefr (refr))
(struct cmd-reify-sturdyrefr (sturdy-refr))

(define eddsa-impl
  (delay/sync (get-pk 'eddsa (crypto-factories))))

(define (make-eddsa-private-key)
  (generate-private-key (force eddsa-impl) '((curve ed25519))))

;; But what does the machine *do*?
;; Does it need an event loop?
;; Does it talk to the "external world"?
;; It seems obvious that yes, I guess that's the main thing it does,
;; is set up connections to the outside world and route outside
;; connections

;; TODO: If we parameterize this, *when* do we set up the ability
;;   for the machine to be able to speak to the outside world?
;;   especially if this happens through Tor or etc.
;; TODO: Does this need to move into core.rkt?

(define (boot-machine
         #:private-key
         ;; TODO: rename to #:d/s-key ?
         [private-key (delay (make-eddsa-private-key))])
  (define public-key
    (delay
      (pk-key->public-only-key (force private-key))))
  (define public-key-as-bytes
    (delay
      (match (pk-key->datum (force public-key) 'rkt-public)
        [(list 'eddsa 'public 'ed25519 public-key-bytes)
         public-key-bytes])))

  (define (_get-machine-id)
    (force public-key-as-bytes))

  ;; be careful!
  ;; I guess we need this to be able to serialize later
  #;(define (_get-machine-private-key)
    (force private-key))

  (define machine-channel
    (make-async-channel))

  (define running? #t)

  (define (main-loop)
    (thread
     (λ ()
       (with-handlers ([exn:fail?
                        (lambda (err)
                          (display ";;;; Error when attempting to run vat main loop:"
                                   (current-error-port))
                          (display (exn->string err)
                                   (current-error-port))
                          (set! running? #f))])
         (let lp ()
           (match (async-channel-get machine-channel)
             [_ 'no-op])
           (when running? (lp)))))))

  (methods
   [get-machine-id _get-machine-id]))

;; Note that being able to access the current-machine is a POWERFUL
;; capability, since it also lets you access the machine private key?
;; So this is ambiently available to general racket programs but we'll
;; need to lock it up in the dungeon
(define current-machine
  (make-parameter (boot-machine)))

(define (register-vat-with-current-machine vat-key vat-dispatcher)
  'TODO)


(module+ test
  (require rackunit
           "utils/install-factory.rkt")
  (install-default-factories!)
  )
