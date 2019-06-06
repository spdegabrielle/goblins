#lang racket/base

(provide mdisp
         define-mdisp
         spawn-mdisp)

(require "../main.rkt")

;; Simple macro for method-based dispatching

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

(define-syntax mdisp
  (syntax-rules ()
    [(mdisp method-defn ...)
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

(define-syntax-rule (define-mdisp id rest ...)
  (define id (mdisp rest ...)))

(define-syntax-rule (spawn-mdisp rest ...)
  (spawn (mdisp rest ...)))

(module+ test
  (require rackunit
           racket/contract)
  (define-mdisp an-mdisp
    [(beep)
     'beep-boop]
    [(hello name)
     (format "hello ~a!" name)]
    [(sing singer [lyric "once upon a bonnie moon..."]
           #:note-str [note-str "o/~"])
     (format "<~a> ~a ~a ~a"
             singer note-str lyric note-str)])

  (check-eq?
   (an-mdisp 'beep)
   'beep-boop)
  (check-equal?
   (an-mdisp 'hello "george")
   "hello george!")
  (check-equal?
   (an-mdisp 'sing "frank")
   "<frank> o/~ once upon a bonnie moon... o/~")
  (check-equal?
   (an-mdisp 'sing "george"
             "once upon a swingin' star..."
             #:note-str "♫")
   "<george> ♫ once upon a swingin' star... ♫")
  (check-exn
   any/c
   (lambda ()
     (an-mdisp 'nope)))

  (check-equal?
   ((mdisp [(foo . bar) bar])
    'foo 'bar 'baz)
   '(bar baz)))
