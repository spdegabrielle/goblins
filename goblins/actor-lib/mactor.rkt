#lang racket/base

(provide mactor
         define-mactor
         spawn-mactor)

(require "../core.rkt")

;; Simple macro for method-based dispatching

;;   mactor (Macro Symbol-based Method) like the venetian/italian
;;   feminine plural word of masimo: masime. which means maximum,
;;   greatest, largest etc.
;;     - mickie / alejandro
;;       https://mastodon.host/@alejandro/102227499241736689

;; TODO: this could be optimized a lot more

(define-syntax method-defn-sym
  (syntax-rules ()
    [(_ [(method-name bcom method-args ...) body ...])
     (quote method-name)]
    [(_ [(method-name bcom method-args ... . rest) body ...])
     (quote method-name)]
    [(_ [method-name proc])
     (quote method-name)]))

(define-syntax method-defn-proc
  (syntax-rules ()
    [(_ [(method-name bcom method-args ...) body ...])
     (lambda (bcom method-args ...)
       body ...)]
    [(_ [(method-name bcom method-args ... . rest) body ...])
     (lambda (bcom method-args ... . rest)
       body ...)]
    [(_ [method-name proc])
     proc]))

(define-syntax mactor
  (syntax-rules ()
    [(mactor method-defn ...)
     (make-keyword-procedure
      (lambda (kws kw-args become method . args)
        (define method-proc
          ;; TODO: we should really be using case here
          ;;   but that seems to require more macro-foo to
          ;;   operate correctly...
          (cond
            [(eq? method (method-defn-sym method-defn))
             (method-defn-proc method-defn)] ...
            [else
             (error (format "No such method ~a" method))]))
        (keyword-apply method-proc kws kw-args become args)))]))

(define-syntax-rule (define-mactor id rest ...)
  (define id (mactor rest ...)))

(define-syntax-rule (spawn-mactor rest ...)
  (spawn (mactor rest ...)))

(module+ test
  (require rackunit
           racket/contract)
  (define-mactor objekt
    [(beep _)
     'beep-boop]
    [(hello _ name)
     (format "hello ~a!" name)]
    [(sing _ singer [lyric "once upon a bonnie moon..."]
           #:note-str [note-str "o/~"])
     (format "<~a> ~a ~a ~a"
             singer note-str lyric note-str)])

  (check-eq?
   (objekt 'become-goes-here 'beep)
   'beep-boop)
  (check-equal?
   (objekt 'become-goes-here 'hello "george")
   "hello george!")
  (check-equal?
   (objekt 'become-goes-here 'sing "frank")
   "<frank> o/~ once upon a bonnie moon... o/~")
  (check-equal?
   (objekt 'become-goes-here 'sing "george"
             "once upon a swingin' star..."
             #:note-str "♫")
   "<george> ♫ once upon a swingin' star... ♫")
  (check-exn
   any/c
   (lambda ()
     (objekt 'become-goes-here 'nope)))

  (check-equal?
   ((mactor [(foo bcom . bar) bar])
    'become-goes-here 'foo 'bar 'baz)
   '(bar baz)))
