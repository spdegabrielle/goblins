#lang racket/base

;; TODO: Doesn't work yet; just a stub.

(require "core.rkt"
         racket/match)

(struct vat (actormap

             incoming-messages
             outgoing-messages

             to-intervat-channel
             from-intervat-channel

             ;; So this is going to be an object in the vat/actormap
             ;; that maps swissnum based sturdyrefs to the actual ref
             ;; objects.
             sturdyref-manager

             ;; TODO:
             ;; keydata
             ))


;; (-> vat? vat?)
(define (vat-turn vat)
  'TODO)

;; (-> vat? message? vat?)
(define (vat-turn-message vat message)
  'TODO)

;; Churn through all messages
(define (vat-churn vat)
  'TODO)

;; (-> vat? vat?)
(define (vat-churn! vat)
  'TODO)

;; (-> vat? vat?)
(define (vat-send-outgoing vat)
  'TODO)

;; (-> vat? vat?)
(define (vat-receive-incoming vat)
  'TODO)

;; (-> vat? any/c)
(define (vat-commit! vat)
  'TODO)


;; Runs in its own thread
(define (start-intervat-manager)
  (define connected
    'TODO)
  ;; This one should be a weak map?
  (define disconnected
    'TODO)
  'TODO)

;; Each of these also run in their own threads
(define (start-intervat-connection)
  (define questions
    'TODO)
  (define answers
    'TODO)
  'TODO)

