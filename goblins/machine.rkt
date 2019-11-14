#lang racket/base

(require "core.rkt"
         racket/async-channel
         racket/match

         crypto
         crypto/private/common/base256
         
         [only-in racket/promise delay delay/sync force])

;;; Core commands
;;; 

(struct cmd-install-network (network))
(struct cmd-register-local-vat (vat-conn))
(struct cmd-register-removte-promise (promise-foo))
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
        [(list 'eddsa public ed25519 public-key-bytes)
         public-key-bytes])))

  (define (_get-machine-id)
    (force public-key-as-bytes))

  ;; be careful!
  (define (_get-machine-private-key)
    (force private-key))

  (define (main-loop)
    'TODO)

  'TODO)

(define current-machine
  (make-parameter (boot-machine)))

(define (register-vat-with-current-machine vat-key vat-dispatcher)
  'TODO)


(module+ test
  (require rackunit
           "utils/install-factory.rkt")
  (install-default-factories!)
  )
