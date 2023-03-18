#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define decimal-symbols (hash 2 #\2 1 #\1 0 #\0 -1 #\- -2 #\=))
(define snafu-symbols (hash #\2 2 #\1 1 #\0 0 #\- -1 #\= -2))

(define (decimal->snafu n)
  (define (sd-aux n carry result)
    (cond ((= n 0) (list->string result))
          (else
           (let ((m (+ (remainder n 5) carry)))
             (sd-aux (quotient n 5) (if (> m 2) 1 0)
                     (cons (if (> m 2) (hash-ref decimal-symbols (- m 5))
                               (hash-ref decimal-symbols m)) result))))))
  (sd-aux n 0 '()))

(define (snafu->decimal s)
  (define (ds-aux s result)
    (cond ((not (non-empty-string? s)) result)
          (else
           (ds-aux (substring s 1)
                   (+ result
                      (* (hash-ref snafu-symbols (string-ref s 0))
                         (expt 5 (- (string-length s) 1))))))))
  (ds-aux s 0))

(time (decimal->snafu (apply + (map snafu->decimal input))))