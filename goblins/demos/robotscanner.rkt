#lang racket

;;; goblins -- An actor model implementation for Racket
;;; Copyright © 2016, 2017 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of goblins.
;;;
;;; goblins is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; goblins is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with goblins.  If not, see <http://www.gnu.org/licenses/>.

;;; =====================================================================
;;; Robot Scanner test demo (from XUDD, then 8sync, and now goblins.
;;; 
;;; Here's the premise.  There's a warehouse full of droids, some
;;; infected, and some not.  The SecurityRobot is being sent in to clean
;;; up the mess.  It's capable of sending a message that infected droids
;;; are susceptible to responding in a predictable way.  Once it has
;;; identified that a droid is infected, it shoots it full of holes till
;;; the droid is terminated.  The SecurityRobot goes from room to room
;;; till things are cleared out.
;;; 
;;; Overseeing the operation is the "overseer".  The security robot keeps
;;; the overseer up to date on its progress as it goes.  (For this demo,
;;; the overseer is also responsible for initializing the world and
;;; reporting info back to the user.)
;;; =====================================================================

(require "../actors.rkt")

(define room-structure
  ;; A list of (clean-droids infected-droids)
  '((3 1)
    (0 2)
    (8 5)
    (5 0)
    (2 1)))

(define overseer%
  (class object%
    (super-new)
    (init-field done?)

    (define/public (init-world)
      ;; Porting mostly straight up from super-imperative XUDD code.
      (define previous-room #f)
      (define first-room #f)

      ;; Set up all rooms
      (for ([room-spec room-structure])
        (match room-spec
          [(list clean-droids infected-droids)
           ;; Create this room
           (define room
             (spawn-new warehouse-room%))
           (define (init-droid #:infected infected)
             (define droid
               (spawn-new droid%
                          [infected infected]
                          [room room]))
             (<<- droid 'register-with-room))

           ;; Link rooms.
           ;; Couldn't this just be folded into the warehouse room init?
           (when previous-room
             (<<- previous-room 'set-next-room
                     #:id room)
             (<<- room 'set-previous-room
                     #:id previous-room))

           ;; Set up clean droids in the room
           (for ([i (in-range clean-droids)])
             (init-droid #:infected #f))

           ;; Set up infected droids in the room
           (for ([i (in-range infected-droids)])
             (init-droid #:infected #t))

           (set! previous-room room)
           (when (not first-room)
             (set! first-room room))]))

      ;; Add security robot
      (let ((security-robot
             (spawn-new security-robot%)))
        (<- security-robot 'begin-mission
            #:starting-room first-room
            #:overseer (self))))

    (define/public (transmission #:text text)
      (displayln text))

    (define/public (done!)
      (semaphore-post done?))))


;;; A room full of robots.
(define warehouse-room%
  (class object%
    (super-new)
    (define droids '())
    (define next-room #f)
    (define previous-room #f)

    (define/public (set-next-room #:id id)
      "Set the room following this"
      (set! next-room id))

    (define/public (set-previous-room #:id id)
      "Set the room previous to this"
      (set! previous-room id))

    (define/public (get-next-room)
     "Return a reference to the link following this"
     next-room)

    (define/public (get-previous-room)
      "Return a reference to the link preceding this"
      previous-room)

    (define/public (list-droids)
      "Return a list of all the droid ids we know of in this room"
      droids)

    (define/public (register-droid #:droid-id droid-id)
      "Register a droid as being in this room"
      (set! droids (cons droid-id droids)))))


;;; A droid that may or may not be infected!
;;; What will happen?  Stay tuned!
(define droid%
  (class object%
    (super-new)
    (init-field infected room)
    (define hp 50)

    (define/public (register-with-room)
      "Register ourselves as being in a room"
      (<<- room 'register-droid
              #:droid-id (self))
      (display
       (format "Droid ~a registered with room ~a\n"
               (self)
               room)))

    (define/public (infection-expose)
      "Leak whether or not we're infected to a security droid"
      infected)

    (define/public (get-shot)
      "Get shot by bullets"
      (let* ((damage (random 60))
             (new-hp (- hp damage))
             (alive (> new-hp 0)))
        ;; Set our health to the new value
        (set! hp new-hp)
        (when (not alive)
          (display (format "~a: *Kaboom!*\n" (self)))
          ;;; TODO:
          ;; (self-destruct actor)
          )
        (values new-hp damage alive)))))


(define (droid-status-format droid-id alive damage-taken hp-left)
  (if alive
      (format "Droid ~a shot; taken ~a damage. Still alive... ~a hp left."
              droid-id damage-taken hp-left)
      (format "Droid ~a shot; taken ~a damage. Terminated."
              droid-id
              damage-taken)))


;;; Security robot... designed to seek out and destroy infected droids.


(define security-robot%
  (class object%
    (super-new)
    (define/public (begin-mission #:starting-room starting-room
                                  #:overseer overseer)
      (security-robot-begin-mission starting-room overseer))))

(define (security-robot-begin-mission starting-room overseer)
  ;; used to track the current room / if any rooms are remaining
  (define room starting-room)

  ;; Walk through all rooms, clearing out infected droids
  ;; Continue this whil there's still another room to investigate.
  (let lp ()
    (<- overseer 'transmission
        #:text (format "Entering room ~a..."
                       room))

    ;; Find all droids in this room and exterminate the infected ones.
    (let ((droid-ids (<<- room 'list-droids)))
      (for-each
       (lambda (droid-id)
         (cond
          ;; Looks like it's infected
          ((<<- droid-id 'infection-expose)
           ;; Inform that it's infected
           (<- overseer 'transmission
               #:text (format "~a found to be infected... taking out"
                              droid-id))

           ;; Keep firing till it's dead.
           (let fire-lp ()
             (call-with-values
                 (lambda () (<<- droid-id 'get-shot))
               (lambda (hp-left damage-taken alive)
                 (<- overseer 'transmission
                     #:text (droid-status-format droid-id alive damage-taken hp-left))
                 ;; still alive... keep firing!
                 (when alive
                   (fire-lp))))))

          ;; Not infected... inform and go to the next one
          (else
           (<- overseer 'transmission
               #:text
               (format "~a is clean... moving on."
                       droid-id)))))
       droid-ids))

    ;; Switch to next room, if there is one.
    (set! room (<<- room 'get-next-room))
    (when room
      (lp)))

  ;; Good job everyone!  Shut down the operation.
  (<<- overseer 'transmission
          #:text "Mission accomplished.")
  (<- overseer 'done!))

(define (run-simulation)
  (define done? (make-semaphore))
  (define overseer (spawn-new overseer%
                              [done? done?]))
  (<- overseer 'init-world)
  (semaphore-wait done?))

