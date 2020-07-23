#lang racket

(provide start-server)

(require racket/unix-socket
         goblins
         goblins/captp
         goblins/actor-lib/methods
         pk)

(define (handle-conn ip op bootstrap-actor)
  ;; dunno if this is the right place I guess...
  ;; really one per connection?  maybe excessive?
  (define comms-vat
    (make-vat))

  (define machinetp-ch
    (make-machinetp-thread ip op
                           comms-vat
                           bootstrap-actor))

  'TODO-maybe-or-its-fine-dunno)

(define (^chatroom bcom chatroom-name)
  (define-cell participants-by-name
    #hash())
  (define-cell participants-by-handler
    #hash())

  (methods
   [(join new-participant-name new-participant)
    ($ participants-by-name
       (hash-set ($ participants-by-name)
                 new-participant-name new-participant))
    ($ participants-by-handler
       (hash-set ($ participants-by-handler)
                 new-participant new-participant-name))]
   [(leave sealed-participant-handler)
    'TODO]

   )

  )

(define ((^says-hello bcom))
  "Hello from the server!")

(define (run-server usock-listener)
  (define chatroom-vat
    (make-vat))
  (define chatroom
    (chatroom-vat 'spawn ^says-hello))
  (pk 'server-running)
  (let lp ()
    (sync (handle-evt (unix-socket-accept-evt usock-listener)
                      (match-lambda
                        [(list ip op)
                         (pk 'server-connected 'ip ip 'op op)
                         (handle-conn ip op chatroom)])))
    (lp)))

(define (start-server unix-domain-socket-path)
  (define usock-listener
    (unix-socket-listen unix-domain-socket-path))

  (dynamic-wind
    (const #f)
    (lambda ()
      (run-server usock-listener))
    (lambda ()
      ;; clean up unix domain socket path
      (delete-file unix-domain-socket-path))))
