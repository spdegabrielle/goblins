#lang racket

(require preserves
         crypto
         racket/random)

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

;; ;; Hello, I am A.  Are you B?
;; 3. A->B <signed <handshake1 a-id b-id a->b-nonce> sig>  ; Signs: ['handshake1 <...obj...>]
(struct handshake1 (a-id b-id a->b-nonce)
  #:prefab)

;; ;; Yes, I am B.  Including nonce from 3.  Please sign nonce4 to prove this is you.
;; ;; Nonce3 is included to show that I'm going along.
;; 4. B->A <signed <handshake2 b-id a-id a->b-nonce b->a-nonce> sig>

(struct handshake2 (b-id a-id a->b-nonce b->a-nonce)
  #:prefab)

;; ;; I really am A, no replay attack.  Here are all nonces signed.
;; 5. A->B <signed <handshake3 a-id b-id a->b-nonce b->a-nonce> sig>

(struct handshake3 (a-id b-id a->b-nonce b->a-nonce)
  #:prefab)

;; At this point, both sides can develop the session id:
;; 
;; (sha256 (bytes-append a-id b-id nonce3 nonce4))

(define (derive-session-id a-id b-id a->b-nonce b->a-nonce)
  (digest 'sha256
          (bytes-append a-id b-id a->b-nonce b->a-nonce)))

(module+ test
  (require rackunit
           "../utils/install-factory.rkt")
  (install-default-factories!)
  (define eddsa-impl
    (get-pk 'eddsa (crypto-factories)))
  (define a-privkey
    (generate-private-key eddsa-impl '((curve ed25519))))
  (define a-pubkey
    (pk-key->public-only-key a-privkey))

  (define b-privkey
    (generate-private-key eddsa-impl '((curve ed25519))))
  (define b-pubkey
    (pk-key->public-only-key b-privkey))

  )



(define (pubkey->bytes pubkey)
  (match (pk-key->datum pubkey 'rkt-public)
    [(list 'eddsa 'public 'ed25519 bytes)
     bytes]))

;; listener
(define (a-establish-connection a-privkey b-pubkey in-port out-port)
  (parameterize ([canonicalize-preserves? #f])
    (define a-id
      (pubkey->bytes a-privkey))
    (define b-id
      (pubkey->bytes b-pubkey))
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

    ;; ;; Hello, I am A.  Are you B?
    ;; 3. A->B <signed <handshake1 a-id a->b-nonce> sig>  ; Signs: ['handshake1 <...obj...>]
    (define a->b-nonce
      (crypto-random-bytes 32))
    (define handshake1-obj
      (handshake1 a-id b-id a->b-nonce))
    (define handshake1-sig
      (pk-sign a-privkey (encode handshake1-obj)))
    (define handshake1-signed
      (signed handshake1-obj handshake1-sig))
    (write-preserve handshake1-signed
                    out-port)

    ;; ;; Yes, I am B.  Including nonce from 3.  Please sign nonce4 to prove this is you.
    ;; ;; Nonce3 is included to show that I'm going along.
    ;; 4. B->A <signed <handshake2 b-id a-id a->b-nonce b->a-nonce> sig>
    (match-define (signed (and (handshake2 hs2-b-id hs2-a-id
                                           hs2-a->b-nonce b->a-nonce)
                               hs2)
                          hs2-sig)
      (read-preserve in-port))
    (unless (equal? hs2-b-id b-id)
      (error 'establish-protocol-error
             "Mismatching B id."))
    (unless (equal? hs2-a-id a-id)
      (error 'establish-protocol-error
             "Mismatching A id."))
    (unless (equal? hs2-a->b-nonce a->b-nonce)
      (error 'establish-protocol-error
             "Mismatching a->b nonce."))
    (unless (pk-verify b-pubkey (encode hs2) hs2-sig)
      (error 'establish-protocol-error
             "Signature verification fail for handshake2"))

    ;; ;; I really am A, no replay attack.  Here are all nonces signed.
    ;; 5. A->B <signed <handshake3 a-id b-id a->b-nonce b->a-nonce> sig>
    (define handshake3-obj
      (handshake3 a-id b-id a->b-nonce b->a-nonce))
    (define handshake3-sig
      (pk-sign a-privkey (encode handshake3-obj)))
    (define handshake3-signed
      (signed handshake3-obj handshake3-sig))
    (write-preserve handshake3-signed out-port)

    ;; Ok we made it this far!  Here's the session id
    (derive-session-id a-id b-id a->b-nonce b->a-nonce)))

;; client
(define (b-establish-connection b-privkey in-port out-port)
  (parameterize ([canonicalize-preserves? #f])
    (define b-id
      (pubkey->bytes b-privkey))

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

    ;; ;; Hello, I am A.  Are you B?
    ;; 3. A->B <signed <handshake1 a-id a->b-nonce> sig>  ; Signs: ['handshake1 <...obj...>]
    (match-define (signed (and (handshake1 a-id hs1-b-id a->b-nonce)
                               hs1)
                          hs1-sig)
      (read-preserve in-port))
    (define a-pubkey
      (datum->pk-key (list 'eddsa 'public 'ed25519 a-id)
                     'rkt-public))
    (unless (equal? b-id hs1-b-id)
      (error 'establish-protocol-error
             "Mismatching B id."))
    (unless (pk-verify a-pubkey (encode hs1) hs1-sig)
      (error 'establish-protocol-error
             "Signature verification fail for handshake2"))

    ;; ;; Yes, I am B.  Including nonce from 3.  Please sign nonce4 to prove this is you.
    ;; ;; Nonce3 is included to show that I'm going along.
    ;; 4. B->A <signed <handshake2 b-id a-id a->b-nonce b->a-nonce> sig>
    (define b->a-nonce
      (crypto-random-bytes 32))
    (define handshake2-obj
      (handshake2 b-id a-id a->b-nonce b->a-nonce))
    (define handshake2-sig
      (pk-sign b-privkey (encode handshake2-obj)))
    (define handshake2-signed
      (signed handshake2-obj handshake2-sig))
    (write-preserve handshake2-signed
                    out-port)

    ;; ;; I really am A, no replay attack.  Here are all nonces signed.
    ;; 5. A->B <signed <handshake3 a-id b-id a->b-nonce b->a-nonce> sig>
    (match-define (signed (and (handshake3 hs3-a-id hs3-b-id
                                           hs3-a->b-nonce hs3-b->a-nonce)
                               hs3)
                          hs3-sig)
      (read-preserve in-port))
    (unless (equal? hs3-a-id a-id)
      (error 'establish-protocol-error
             "Mismatching A id."))
    (unless (equal? hs3-b-id b-id)
      (error 'establish-protocol-error
             "Mismatching B id."))
    (unless (equal? hs3-a->b-nonce a->b-nonce)
      (error 'establish-protocol-error
             "Mismatching a->b nonce."))
    (unless (equal? hs3-b->a-nonce b->a-nonce)
      (error 'establish-protocol-error
             "Mismatching b->a nonce."))
    (unless (pk-verify a-pubkey (encode hs3) hs3-sig)
      (error 'establish-protocol-error
             "Signature verification fail for handshake3"))

    ;; At this point, both sides can develop the session id
    (derive-session-id a-id b-id a->b-nonce b->a-nonce)))

(module+ test
  (define-values (a->b-input a->b-output)
    (make-pipe #f 'a->b-input 'a->b-output))
  (define-values (b->a-input b->a-output)
    (make-pipe #f 'b->a-input 'b->a-output))

  

  )

