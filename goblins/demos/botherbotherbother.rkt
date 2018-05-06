#lang racket

(require "../actors.rkt"
         racket/random)

(define student-names
  '("Henry" "Harmony" "Rolf"))

(define (student-name-generator)
  ;; a hashmap of student names with their current count
  (define student-count (make-hash))
  (lambda ()
    (let* ((student (random-ref student-names))
           (current-number (hash-ref student-count student 1)))
      (hash-set! student-count student (+ current-number 1))
      (format "~a-~a" student current-number))))

(define student%
  (class object%
    (super-new)
    (init-field name)
    (define dead? #f)
    (define/public (bother-professor target)
      "Go bother a professor"
      (displayln (format "~a: Bother bother bother!"
                         name))
      (<- target 'be-bothered
          (self) #:noise "Bother bother bother!\n")
      ;; requeue ourselves
      (when (not dead?)
        (<- (self) 'bother-professor target)))
    ;; This kills the student.
    (define/public (be-lambda-consvardraed)
      (display
       (format "~a says: AAAAAAAHHHH!!! I'm dead!\n"
               name))
      (set! dead? #t))
    (define/public (are-you-dead)
      dead?)))

(define complaints
  '("Hey!" "Stop that!" "Oof!"))

(define professor%
  (class object%
    (super-new)
    (define whos-bothering
      (make-hasheq))
    (define/public (be-bothered botherer #:noise noise)
      (hash-set! whos-bothering botherer #t)

      ;; Oof!  Those kids!
      (display (string-append "Professor: "
                              (random-ref complaints)))
      (newline)

      ;; More than one student is bothering us, lose our temper
      (if (> (hash-count whos-bothering) 1)
          (begin
            (display "Professor: LAMBDA CONSVARDRA!\n")
            (for (([student _] whos-bothering))
              (<- student 'be-lambda-consvardrad)
              ;; Remove student from bothering list
              (hash-remove! whos-bothering student)))
          ;; Otherwise, remove them from the list and carry on
          (hash-remove! whos-bothering botherer)))))

(define (run-simulation [num-students 10])
  (define professor (spawn-new professor%))
  (define namegen (student-name-generator))
  (define students
    (for/list ([i (in-range num-students)])
      (spawn-new student%
                 [name (namegen)])))
  (for ([student students])
    (student 'bother-professor professor))
  ;; in other words, this program doesn't really halt
  (semaphore-wait (make-semaphore)))

(module+ main
  (run-simulation))
