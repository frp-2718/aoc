#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (parse input)
  (filter (lambda (e) (not (eof-object? e)))
          (map (lambda (s)
                 (with-input-from-string
                     (regexp-replaces s
                                      '((#rx"\\[" "(") (#rx"\\]" ")") (#rx"," " ")))
                   read))
               input)))

(define (compare e1 e2)
  (cond ((and (list? e1) (list? e2))
         (cond ((and (null? e1) (null? e2)) 0)
               ((null? e1) -1)
               ((null? e2) 1)
               (else (let ((res (compare (car e1) (car e2))))
                       (if (= res 0) (compare (cdr e1) (cdr e2)) res)))))
        ((list? e1) (compare e1 (list e2)))
        ((list? e2) (compare (list e1) e2))
        (else (cond ((< e1 e2) -1)
                    ((> e1 e2) 1)
                    (else 0)))))

(define (build-lists l res)
  (cond ((null? l) (reverse res))
        (else (build-lists (drop l 2) (cons (take l 2) res)))))

(define (compare-all l)
  (define (comp-index l i res)
    (cond ((null? l) res)
          (else (let ((sig (car l)))
                  (comp-index (cdr l) (+ i 1) (if (< (compare (car sig) (cadr sig)) 0) (cons i res) res))))))
  (comp-index l 1 '()))

(define (gt? l1 l2)
  (> 0 (compare l1 l2)))

(define marker-1 "[[2]]")
(define marker-2 "[[6]]")

(define (sort-packets input)
  (sort (parse (append (list marker-1 marker-2) input)) gt?))

(define (find-index l m1 m2)
  (let ((markers (parse (list m1 m2)))
        (vec (list->vector l)))
    (list (+ 1 (vector-member (car markers) vec)) (+ 1 (vector-member (cadr markers) vec)))))

(time (begin (displayln (apply + (compare-all (build-lists (parse input) '()))))
             (displayln (apply * (find-index (sort-packets input) marker-1 marker-2)))))
