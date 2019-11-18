#lang racket

(provide sealed-box
         seal-box unseal-box)

(require crypto)

(struct sealed-box (type 1off-key encrypted)
  #:prefab)

;; From RFC7748:
;;
;; Both now share K = X25519(a, X25519(b, 9)) = X25519(b, X25519(a, 9))
;; as a shared secret.  Both MAY check, without leaking extra
;; information about the value of K, whether K is the all-zero value and
;; abort if so (see below).  Alice and Bob can then use a key-derivation
;; function that includes K, K_A, and K_B to derive a symmetric key.
;;
;; The check for the all-zero value results from the fact that the
;; X25519 function produces that value if it operates on an input
;; corresponding to a point with small order, where the order divides
;; the cofactor of the curve (see Section 7).  The check may be
;; performed by ORing all the bytes together and checking whether the
;; result is zero, as this eliminates standard side-channels in software
;; implementations.

(define (ensure-not-all-zeroes key)
  (when (zero?
         (for/fold ([result 0])
                   ([b key])
           (bitwise-ior result b)))
    (error 'all-zero-key)))

(define (smushed-iv bytes1 bytes2)
  (subbytes (sha256-bytes (bytes-append bytes1 bytes2))
            0 16))


(define/contract (seal-box to-key msg)
  (-> (and/c pk-key? pk-can-key-agree?) bytes? sealed-box?)
  (define ecx-impl
    (get-pk 'ecx (crypto-factories)))
  (define 1off-privkey
    (generate-private-key ecx-impl '((curve x25519))))
  (define 1off-pubkey-bytes
    (match (pk-key->datum 1off-privkey 'rkt-public)
      [(list 'ecx 'public 'x25519 pub)
       pub]))

  (define to-key-bytes
    (match (pk-key->datum to-key 'rkt-public)
      [(list 'ecx 'public 'x25519 pub)
       pub]))

  (define shared-symmetric-key
    (pk-derive-secret 1off-privkey to-key))

  ;; Protect against all-zero symmetric key
  (ensure-not-all-zeroes shared-symmetric-key)

  ;; TODO: maybe an all-zero iv is actually fine?
  ;;   I don't see any reason at all to generate an IV
  ;;   and yet it looks like libsodium is doing so
  (define iv
    (smushed-iv to-key-bytes 1off-pubkey-bytes))

  ;; TODO: Should we use chacha20-poly1305 instead?
  ;;   That should help us more accurately identify an unsealing error.
  (define encrypted
    (encrypt '(aes ctr) shared-symmetric-key
             iv msg
             #:pad #t))

  (sealed-box 'x25519 1off-pubkey-bytes encrypted))

(define/contract (unseal-box to-key _sealed-box)
  (-> (and/c pk-key? pk-can-key-agree?) sealed-box? bytes?)
  (match-define (sealed-box 'x25519 1off-key-bytes encrypted)
    _sealed-box)

  (define to-key-bytes
    (match (pk-key->datum to-key 'rkt-public)
      [(list 'ecx 'public 'x25519 pub)
       pub]))

  (define 1off-key
    (datum->pk-key (list 'ecx 'public 'x25519 1off-key-bytes)
                   'rkt-public))

  (define shared-symmetric-key
    (pk-derive-secret to-key 1off-key))

  ;; Protect against all-zero symmetric key
  (ensure-not-all-zeroes shared-symmetric-key)

  ;; TODO: see iv comment in seal-box
  (define iv
    (smushed-iv to-key-bytes 1off-key-bytes))

  (decrypt '(aes ctr) shared-symmetric-key iv encrypted
           #:pad #t))

(module+ test
  (require rackunit
           "../utils/install-factory.rkt")
  (install-default-factories!)

  (define ecx-impl
    (get-pk 'ecx (crypto-factories)))
  (define ecx-privk1
    (generate-private-key ecx-impl '((curve x25519))))
  (define ecx-pubk1
    (pk-key->public-only-key ecx-privk1))

  (define sealed-box1
    (seal-box ecx-pubk1 #"hello my friend"))
  
  (test-equal?
   "Box sealing/unsealing works"
   (unseal-box ecx-privk1 sealed-box1)
   #"hello my friend"))
