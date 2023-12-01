#lang racket

(define input (file->string "../test-inputs/01-test.txt"))

(define (sums s)
  (define (convert ls)
    (map string->number ls))
  (map (lambda (str)
         (apply + (convert (string-split str "\n"))))
       (string-split s "\n\n")))

(time (begin
        (let ((sorted (sort (sums input) >)))
          (displayln (car sorted))
          (displayln (apply + (take sorted 3))))))
