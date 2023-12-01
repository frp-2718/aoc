#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (parse-intervals str)
  (let ((intervals (string-split str ",")))
    (map string->interval intervals)))

(define (string->interval str)
  (let ((numbers (string-split str "-")))
    (map string->number numbers)))

(define (lower interval)
  (car interval))

(define (upper interval)
  (cadr interval))

(define (contained int-1 int-2)
  (or
   (and (>= (lower int-2) (lower int-1))
        (<= (upper int-2) (upper int-1)))
   (and (>= (lower int-1) (lower int-2))
        (<= (upper int-1) (upper int-2)))))

(define (overlap int-1 int-2)
  (or
   (and (<= (lower int-2) (lower int-1))
        (>= (upper int-2) (lower int-1)))
   (and (<= (lower int-1) (lower int-2))
        (>= (upper int-1) (lower int-2)))))

(define (count f intervals)
  (for/sum ((i intervals))
    (if (f (car i) (cadr i)) 1 0)))

(time
 (let ((intervals (map parse-intervals input)))
   (begin (displayln (count contained intervals))
          (displayln (count overlap intervals)))))
