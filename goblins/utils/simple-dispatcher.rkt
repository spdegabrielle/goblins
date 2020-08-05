#lang racket/base

(provide define-simple-dispatcher)

(define-syntax-rule (define-simple-dispatcher id [method-name method-handler] ...)
  (define id
    (procedure-rename
     (make-keyword-procedure
      (λ (kws kw-vals this-method-name . args)
        (define method
          (case this-method-name
            ['method-name method-handler] ...
            [else (error 'connector-dispatcher-error
                         "Unnown method: ~a" this-method-name)]))
        (keyword-apply method kws kw-vals args)))
     'id)))
