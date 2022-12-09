#lang racket

(define (solve datastream marker)
  (for/first ([i (in-range (- (string-length datastream) marker))]
              #:when (not (check-duplicates
                            (string->list (substring datastream i (+ i marker))))))
      (+ marker i)))

(define (part1 input)
  (for-each (lambda (d)
              (println (solve d 4)))
            input))

(define (part2 input)
  (for-each (lambda (d)
              (println (solve d 14)))
            input))

(time (begin (define input (file->lines "input.txt"))
             (part1 input)
             (part2 input)))