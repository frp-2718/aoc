#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (is-set? l)
  (= (length l) (length (set->list (list->set l)))))

(define (str-set? str)
  (is-set? (string->list str)))

(define (first-pos str cur-pos len)
  (cond ((= cur-pos (- (string-length str) len)) -1)
        ((str-set? (substring str cur-pos (+ cur-pos len))) (+ cur-pos len))
        (else (first-pos str (+ cur-pos 1) len))))

(define (find-pos lines mlen)
  (for ((l lines))
    (displayln (first-pos l 0 mlen))))

(time (begin (find-pos input 4)
             (find-pos input 14)))
