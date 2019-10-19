#lang racket/base

(provide mactor)

(require "../core.rkt"
         racket/match)

;; Simple macro for method-based dispatching

;;   mactor (Macro Symbol-based Method) like the venetian/italian
;;   feminine plural word of masimo: masime. which means maximum,
;;   greatest, largest etc.
;;     - mickie / alejandro
;;       https://mastodon.host/@alejandro/102227499241736689

;; TODO: this could be optimized a lot more

(define-syntax method-defn-sym
  (syntax-rules ()
    [(_ [(method-name method-args ...) body ...])
     (quote method-name)]
    [(_ [(method-name method-args ... . rest) body ...])
     (quote method-name)]
    [(_ [method-name proc])
     (quote method-name)]))

(define-syntax method-defn-proc
  (syntax-rules ()
    [(_ [(method-name method-args ...) body ...])
     (lambda (method-args ...)
       body ...)]
    [(_ [(method-name method-args ... . rest) body ...])
     (lambda (method-args ... . rest)
       body ...)]
    [(_ [method-name proc])
     proc]))

(define-syntax mactor
  (syntax-rules ()
    [(mactor method-defn ...)
     (make-keyword-procedure
      (lambda (kws kw-args . args)
        (match args
          [(cons method non-method-args)
           (define method-proc
             ;; TODO: we should really be using case here
             ;;   but that seems to require more macro-foo to
             ;;   operate correctly...
             (cond
               [(eq? method (method-defn-sym method-defn))
                (method-defn-proc method-defn)] ...
               [else
                (error (format "No such method ~a" method))]))
           (keyword-apply method-proc kws kw-args non-method-args)]
          [_ (error "Method required for mactor method dispatch call")])))]))

(module+ test
  (require rackunit
           racket/contract)
  (define objekt
    (mactor
     [(beep)
      'beep-boop]
     [(hello name)
      (format "hello ~a!" name)]
     [(sing singer [lyric "once upon a bonnie moon..."]
            #:note-str [note-str "o/~"])
      (format "<~a> ~a ~a ~a"
              singer note-str lyric note-str)]))

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
   ((mactor [(foo . bar) bar])
    'foo 'bar 'baz)
   '(bar baz)))
