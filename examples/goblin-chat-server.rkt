#lang racket

(provide start-server)

(require racket/unix-socket
         goblins
         goblins/captp
         goblins/actor-lib/methods
         syrup
         pk)

(define (handle-conn ip op)
  (pk 'conn-thread)
  (let lp ()
    (call/ec
     (lambda (return-early)
       (match (syrup-read ip)
         [(? eof-object?)
          (pk 'client-disconnect)
          (return-early)]
         [msg
          (pk 'client-sez msg)])
       (lp)))))

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

(define (run-server usock-listener)
  (define comms-vat
    (make-vat))
  (define chatroom-vat
    (make-vat))
  (let lp ()
    (sync (handle-evt (unix-socket-accept-evt usock-listener)
                      (match-lambda
                        [(list ip op)
                         (pk 'server-connected 'ip ip 'op op)
                         (thread (lambda () (handle-conn ip op)))])))
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
