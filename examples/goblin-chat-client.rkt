#lang racket

(provide start-client)

(require racket/unix-socket
         racket/async-channel
         goblins/captp
         goblins
         pk)


(define ((^says-hello bcom))
  "Hello from the client!")

(define (start-client unix-domain-socket-path)
  (define-values (ip op)
    (unix-socket-connect unix-domain-socket-path))
  (pk 'client-connected 'ip ip 'op op)
  (define comms-vat
    (make-vat))
  (define client-vat
    (make-vat))
  (define bs-actor
    (client-vat 'spawn ^says-hello))
  (define machinetp-ch
    (make-machinetp-thread ip op
                           comms-vat
                           bs-actor))
  (define bootstrap-vow
    (let ([return-ch (make-channel)])
      (async-channel-put machinetp-ch
                         (vector 'get-bootstrap-promise return-ch))
      (channel-get return-ch)))

  (client-vat 'run
              (lambda ()
                (on (<- bootstrap-vow)
                    (lambda (val)
                      (pk 'client-got-back val))))))

