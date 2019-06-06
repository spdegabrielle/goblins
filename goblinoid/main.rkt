#lang racket/base

(provide make-actormap
         snapshot-actormap hasheq->actormap
         transactormap-merge!
         actormap? transactormap? actormappable?

         actormap-turn
         actormap-poke!
         actormap-peek
         actormap-turn-message
         actormap-run
         actormap-run*
         actormap-run!
         actormap-spawn
         actormap-spawn!

         call spawn <-

         (struct-out message)

         near-ref? far-ref?)

(require "core.rkt"
         "message.rkt")
