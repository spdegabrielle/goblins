#lang racket

(require racket/unix-socket
         pk
         "goblin-chat-server.rkt"
         "goblin-chat-client.rkt")

(module+ main
  (define server? #f)
  (define unix-domain-socket-path
    (command-line
     #:once-any
     [("--server") "Start a server"
                   (set! server? #t)]
     [("--client") "Start a client (the default)"
                   (set! server? #f)]
     #:args (unix-domain-socket-path)
     unix-domain-socket-path))

  (cond
    [server?
     (start-server unix-domain-socket-path)]
    [#t
     (start-client unix-domain-socket-path)]))
