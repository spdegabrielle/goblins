#lang racket

(require preserves
         crypto
         racket/random
         "sealed-box.rkt")

;;; This whole protocol for setting up a session is based on the idea that two
;;; machines will want to establish communication with one another and will want
;;; a "session id" to continuously update for each machine they talk to.
;;;
;;; However do we ever want to be able to "forget about" an old
;;; machine?  If so, then we'll want to be able to reset counting up
;;; the numbers we're keeping track of.
;;;
;;; This is probably premature optimization... an alternative is we
;;; can just keep around all machines we've ever seen.  However the
;;; record keeping to do that for eg the entire ipv4 space would be
;;; about 192GB I think?
;;;
;;; I guess this is a useful exercise to experiment with though...


;;; Prefab structs
;;; ==============

(struct signed (obj sig)
  #:prefab)

;; 1. A->B <protocol bidir-pipe>

(struct initiate-protocol (proto-name)
  #:prefab)

;; 2. B->A <accept-protocol bidir-pipe>
(struct accept-protocol (proto-name)
  #:prefab)

;; ;; Hello, I am A.  Are you B?  Also here's a throwaway key to respond with.
;; 3. A->B <signed <handshake1 a-id b-id tmp-seal-key> sig>  ; Signs: ['handshake1 <...obj...>]
(struct handshake1 (a-id b-id tmp-seal-key)
  #:prefab)

;; ;; Yes, I am B.
;; ;; Here is the hash of the last message so you know this is a proper reply.
;; ;; Here is the key you should use for me (sealed to tmp-seal-key)
;; 4. B->A <signed <handshake2 hash-of-hs1 #base64{sealed b-seal-key}> sig>

(struct handshake2 (hash-of-hs1 sealed-b-seal-key)
  #:prefab)

;; ;; I really am A, no replay attack.
;; ;; Here is the hash of the last message so you know this is a proper reply.
;; ;; Here is the key you should use for me (sealed to b-seal-key).
;; 5. A->B <signed <handshake3 hash-of-hs2 #base64{sealed a-seal-key}> sig>

(struct handshake3 (hash-of-hs2 sealed-a-seal-key)
  #:prefab)

;; At this point, both sides can develop the session id:
;;   (sha256 (canonicalized (msg-5)))

(define (veri-key->bytes veri-key)
  (match (pk-key->datum veri-key 'rkt-public)
    [(list 'eddsa 'public 'ed25519 bytes)
     bytes]))

(define (seal-key->bytes enc-key)
  (match (pk-key->datum enc-key 'rkt-public)
    [(list 'ecx 'public 'x25519 pub-bytes)
     pub-bytes]))


(define (bytes->seal-key pub-bytes)
  (datum->pk-key (list 'ecx 'public 'x25519 pub-bytes)
                 'rkt-public))

(define (sign obj sign-key)
  (signed obj (pk-sign sign-key (encode obj))))

;; Verify signed object against expected verification key.
;; If signature passes, return the object.
(define (get-verified-obj signed-obj verify-key)
  (match signed-obj
    [(signed obj (? bytes? sig))
     (unless (pk-verify verify-key (encode obj) sig)
       (error 'signature-verification-failure signed-obj))
     obj]))

;; Ensure that a preserves object matches the expected hash.
;; If not, throw an error.
(define (ensure-matching-hash obj sha256-hash)
  (unless (equal? (sha256-bytes (encode obj))
                  sha256-hash)
    (error 'hash-mismatch)))

(define (make-seal/unseal-key)
  (define ecx-impl
    (get-pk 'ecx (crypto-factories)))
  (generate-private-key ecx-impl '((curve x25519))))

;; listener
(define (a-establish-connection a-signkey b-veri-key in-port out-port)
  (parameterize ([canonicalize-preserves? #t])
    (define a-id
      (veri-key->bytes a-signkey))
    (define b-id
      (veri-key->bytes b-veri-key))
    ;; 1. A->B <protocol bidir-pipe>
    (write-preserve (initiate-protocol 'bidir-pipe)
                    out-port)


    ;; 2. B->A <accept-protocol bidir-pipe>
    (match (read-preserve in-port)
      [(accept-protocol 'bidir-pipe)
       'ok]
      [unexpected-response
       (error 'establish-protocol-error
              "unexpected-response: ~a"
              unexpected-response)])

;; ;; Hello, I am A.  Are you B?  Also here's a throwaway key to respond with.
;; 3. A->B <signed <handshake1 a-id b-id tmp-seal-key> sig>  ; Signs: ['handshake1 <...obj...>]
    (define tmp-seal/unseal-key
      (make-seal/unseal-key))
    (define hs1-obj
      (handshake1 a-id b-id (seal-key->bytes tmp-seal/unseal-key)))
    (define hs1-signed
      (sign hs1-obj a-signkey))
    (write-preserve hs1-signed out-port)

    ;; ;; Yes, I am B.
    ;; ;; Here is the hash of the last message so you know this is a proper reply.
    ;; ;; Here is the key you should use for me (sealed to tmp-seal-key)
    ;; 4. B->A <signed <handshake2 hash-of-3 #base64{sealed b-seal-key}> sig>
    (define hs2
      (read-preserve in-port))
    (define hs2-obj
      (get-verified-obj hs2 b-veri-key))
    (match-define (handshake2 hash-of-hs1 sealed-b-seal-key)
      hs2-obj)
    (ensure-matching-hash hs1-signed hash-of-hs1)
    (define b-seal-key-bytes
      (unseal-box tmp-seal/unseal-key sealed-b-seal-key))
    (define b-seal-key
      (bytes->seal-key b-seal-key-bytes))

    ;; ;; I really am A, no replay attack.
    ;; ;; Here is the hash of the last message so you know this is a proper reply.
    ;; ;; Here is the key you should use for me (sealed to b-seal-key).
    ;; 5. A->B <signed <handshake3 hash-of-hs2 #base64{sealed a-seal-key}> sig>
    (define a-seal/unseal-key
      (make-seal/unseal-key))
    (define sealed-a-seal-key
      (seal-box b-seal-key (seal-key->bytes a-seal/unseal-key)))
    (define hs3-obj
      (handshake3 (sha256-bytes (encode hs2))
                  sealed-a-seal-key))
    (define hs3-signed
      (sign hs3-obj a-signkey))
    (write-preserve hs3-signed out-port)

    ;; Ok we made it this far!  Here's the session id... its' just the hash of
    ;; hs3-signed
    (sha256-bytes (encode hs3-signed))))

;; client
(define (b-establish-connection b-signkey in-port out-port)
  (parameterize ([canonicalize-preserves? #t])
    (define b-id
      (veri-key->bytes b-signkey))

    ;; 1. A->B <protocol bidir-pipe>
    (match (read-preserve in-port)
      [(initiate-protocol 'bidir-pipe)
       'ok]
      [unexpected-response
       (error 'establish-protocol-error
              "unexpected protocol or initialization request: ~a"
              unexpected-response)])

    ;; 2. B->A <accept-protocol bidir-pipe>
    (write-preserve (accept-protocol 'bidir-pipe)
                    out-port)

    ;; ;; Hello, I am A.  Are you B?  Also here's a throwaway key to respond with.
    ;; 3. A->B <signed <handshake1 a-id b-id tmp-seal-key> sig>  ; Signs: ['handshake1 <...obj...>]
    (define hs1 (read-preserve in-port))
    (match-define (signed (and (handshake1 a-id hs1-b-id tmp-seal-key-bytes)
                               hs1-obj)
                          hs1-sig)
      hs1)
    (define a-veri-key
      (datum->pk-key (list 'eddsa 'public 'ed25519 a-id)
                     'rkt-public))
    (unless (equal? b-id hs1-b-id)
      (error 'establish-protocol-error
             "Mismatching B id."))
    ;; Done for its check side-effect
    (get-verified-obj hs1 a-veri-key)
    (define tmp-seal-key
      (bytes->seal-key tmp-seal-key-bytes))

    ;; ;; Yes, I am B.
    ;; ;; Here is the hash of the last message so you know this is a proper reply.
    ;; ;; Here is the key you should use for me (sealed to tmp-seal-key)
    ;; 4. B->A <signed <handshake2 hash-of-hs1 #base64{sealed b-seal-key}> sig>
    (define b-seal/unseal-key
      (make-seal/unseal-key))
    (define sealed-b-seal-key
      (seal-box tmp-seal-key (seal-key->bytes b-seal/unseal-key)))
    (define hs2-obj
      (handshake2 (sha256-bytes (encode hs1))
                  sealed-b-seal-key))
    (define hs2-signed
      (sign hs2-obj b-signkey))
    (write-preserve hs2-signed out-port)

    ;; ;; I really am A, no replay attack.
    ;; ;; Here is the hash of the last message so you know this is a proper reply.
    ;; ;; Here is the key you should use for me (sealed to b-seal-key).
    ;; 5. A->B <signed <handshake3 hash-of-hs2 #base64{sealed a-seal-key}> sig>
    (define hs3-signed
      (read-preserve in-port))
    (define hs3-obj
      (get-verified-obj hs3-signed a-veri-key))
    (match-define (handshake3 hash-of-hs2 sealed-a-seal-key)
      hs3-obj)
    (ensure-matching-hash hs2-signed hash-of-hs2)
    (define a-seal-key-bytes
      (unseal-box b-seal/unseal-key sealed-a-seal-key))
    (define a-seal-key
      (bytes->seal-key a-seal-key-bytes))

    ;; At this point, both sides can develop the session id:
    ;;   (sha256 (canonicalized (msg-5)))
    (sha256-bytes (encode hs3-signed))))

(module+ test
  (require rackunit
           "../utils/install-factory.rkt")
  (install-default-factories!)
  (define eddsa-impl
    (get-pk 'eddsa (crypto-factories)))
  (define a-signkey
    (generate-private-key eddsa-impl '((curve ed25519))))
  (define a-veri-key
    (pk-key->public-only-key a-signkey))

  (define b-signkey
    (generate-private-key eddsa-impl '((curve ed25519))))
  (define b-veri-key
    (pk-key->public-only-key b-signkey))

  (define-values (a->b-input a->b-output)
    (make-pipe #f 'a->b-input 'a->b-output))
  (define-values (b->a-input b->a-output)
    (make-pipe #f 'b->a-input 'b->a-output))

  
  #;(thread (lambda () (displayln (format "a session: ~a" (a-establish-connection a-signkey b-veri-key b->a-input a->b-output)))))
  #;(thread (lambda () (displayln (format "b session: ~a" (b-establish-connection b-signkey a->b-input b->a-output)))))

  )

