#lang racket

(define input (map (lambda (s) (string-split s " "))
                   (file->lines (command-line #:args (filename) filename))))

(define (cd-down? cmd)
  (and (string=? (cadr cmd) "cd")
       (not (string=? (caddr cmd) ".."))))

(define (cd-up? cmd)
  (and (string=? (cadr cmd) "cd")
       (string=? (caddr cmd) "..")))

(define (size? cmd)
  (string->number (car cmd)))

(define (parse input)
  (define (parse-aux input stack sizes)
    (if (null? input)
        (append stack sizes)
        (let ((cmd (car input)))
          (cond ((cd-down? cmd) (parse-aux (cdr input) (cons 0 stack) sizes))
                ((cd-up? cmd) (parse-aux (cdr input) (cdr stack) (cons (car stack) sizes)))
                ((size? cmd) (parse-aux (cdr input)
                                        (map (lambda (s) (+ s (string->number (car cmd)))) stack) sizes))
                (else (parse-aux (cdr input) stack sizes))))))
  (parse-aux input '() '()))

(define sizes (sort (parse input) >))

(define (free-space sizes)
  (let* ((disk-space 70000000)
         (needed-space 30000000)
         (unused-space (- disk-space (car sizes)))
         (space-to-free (- needed-space unused-space)))
    (car (sort (filter (lambda (x) (>= x space-to-free)) sizes) <))))
    
(time (begin (displayln (apply + (filter (lambda (n) (<= n 100000)) sizes)))
             (displayln (free-space sizes))))