#lang racket/base

(provide symethods
         define-symethods
         spawn-symethods)

(require "../core.rkt")

;; Simple macro for method-based dispatching

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

(define-syntax symethods
  (syntax-rules ()
    [(symethods method-defn ...)
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

(define-syntax-rule (define-symethods id rest ...)
  (define id (symethods rest ...)))

(define-syntax-rule (spawn-symethods rest ...)
  (spawn (symethods rest ...)))

(module+ test
  (require rackunit
           racket/contract)
  (define-symethods objekt
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
   ((symethods [(foo . bar) bar])
    'foo 'bar 'baz)
   '(bar baz)))
