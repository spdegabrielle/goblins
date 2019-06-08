#lang racket/base

(provide masyme
         define-masyme
         spawn-masyme)

(require "../core.rkt")

;; Simple macro for method-based dispatching

;;   masyme (Macro Symbol-based Method) like the venetian/italian
;;   feminine plural word of masimo: masime. which means maximum,
;;   greatest, largest etc.
;;     - mickie / alejandro
;;       https://mastodon.host/@alejandro/102227499241736689

;; TODO: this could be optimized a lot more

(define-syntax do-method-defn
  (syntax-rules ()
    [(_ [(method-name method-args ...) body ...])
     (cons (quote method-name)
           (lambda (method-args ...)
             body ...))]
    [(_ [(method-name method-args ... . rest) body ...])
     (cons (quote method-name)
           (lambda (method-args ... . rest)
             body ...))]
    [(_ [method-name proc])
     (cons (quote method-name) proc)]))

(define-syntax masyme
  (syntax-rules ()
    [(masyme method-defn ...)
     (let ([methods
            (make-hasheq
             (list (do-method-defn method-defn) ...))])
       (make-keyword-procedure
        (lambda (kws kw-args method . args)
          (define method-proc
            (hash-ref methods method #f))
          (unless method-proc
            (error (format "No such method ~a" method)))
          (keyword-apply method-proc kws kw-args args))))]))

(define-syntax-rule (define-masyme id rest ...)
  (define id (masyme rest ...)))

(define-syntax-rule (spawn-masyme rest ...)
  (spawn (masyme rest ...)))

(module+ test
  (require rackunit
           racket/contract)
  (define-masyme objekt
    [(beep)
     'beep-boop]
    [(hello name)
     (format "hello ~a!" name)]
    [(sing singer [lyric "once upon a bonnie moon..."]
           #:note-str [note-str "o/~"])
     (format "<~a> ~a ~a ~a"
             singer note-str lyric note-str)])

  (check-eq?
   (objekt 'beep)
   'beep-boop)
  (check-equal?
   (objekt 'hello "george")
   "hello george!")
  (check-equal?
   (objekt 'sing "frank")
   "<frank> o/~ once upon a bonnie moon... o/~")
  (check-equal?
   (objekt 'sing "george"
             "once upon a swingin' star..."
             #:note-str "♫")
   "<george> ♫ once upon a swingin' star... ♫")
  (check-exn
   any/c
   (lambda ()
     (objekt 'nope)))

  (check-equal?
   ((masyme [(foo . bar) bar])
    'foo 'bar 'baz)
   '(bar baz)))
