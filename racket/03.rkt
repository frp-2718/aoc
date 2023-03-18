#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (found-char-table) (build-vector 128 (lambda (x) (list x #f))))

(define (find-char str)
  (let ((table (found-char-table)))
    (for ((c str))
      (let ((index (char->integer c)))
        (vector-set! table index (list (car (vector-ref table index)) #t))))
    table))

(define (unique strs)
  (let ((res (find-char (car strs))))
    (for ((str (cdr strs)))
      (set! res (vector-map
                 (lambda (a b) (list (car a) (and (cadr a) (cadr b))))
                 res
                 (find-char str))))
  (integer->char (car (vector-ref (vector-filter
                                    (lambda (p) (cadr p)) res) 0)))))

(define (priority c)
  (cond ((char-upper-case? c) (+ (- (char->integer c) (char->integer #\A)) 27))
        (else (+ (- (char->integer c) (char->integer #\a)) 1))))

(define (priorities strs)
  (define (compute-priorities strs total)
    (cond ((null? strs) total)
          (else (compute-priorities (cdr strs)
                                    (+ total (priority (unique (car strs))))))))
  (compute-priorities strs 0))

; part 1
(define (split s)
  (let ((half (/ (string-length s) 2)))
    (list (substring s 0 half) (substring s half))))

; part 2
(define (group-three l)
  (cond ((null? l) '())
        (else (cons (list (car l) (cadr l) (caddr l))
                    (group-three (cdddr l))))))

(time (begin (displayln (priorities (map split input)))
             (displayln (priorities (group-three input)))))
