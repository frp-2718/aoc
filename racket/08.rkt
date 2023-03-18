#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (lines->matrix ls)
  (for/list ((line ls))
    (for/list ((c line))
      (list (string->number (string c)) #f))))

(define (lines->matrix2 ls)
  (for/list ((line ls))
    (for/list ((c line))
      (string->number (string c)))))

(define (transpose matrix)
  (apply map list matrix))

(define (reverse-matrix m)
  (map reverse m))

(define (visibles-row row)
  (define (row-aux row res max)
    (cond ((null? row) res)
          ((> (caar row) max) (row-aux (cdr row) (cons (list (caar row) #t) res) (caar row)))
          (else (row-aux (cdr row) (cons (car row) res) max))))
  (reverse (row-aux row '() -1)))

(define (set-visibles grid)
  (for/list ((line grid))
    (visibles-row line)))

(define (visibles grid)
  (length (filter identity (map second (apply append grid)))))

; part 1
(define (part1 input)
  (let* ((matrix (lines->matrix input))
         (with-visibles (set-visibles (reverse-matrix
                                      (set-visibles (transpose
                                                    (set-visibles (reverse-matrix
                                                                  (set-visibles matrix)))))))))
    (visibles with-visibles)))
    
; part 2
(define (subscore lst i)
  (define (subscore-aux height l score)
    (cond ((null? l) score)
          ((>= (car l) height) (+ 1 score))
          (else (subscore-aux height (cdr l) (+ 1 score)))))
  (let ((l (drop lst i)))
    (subscore-aux (car l) (cdr l) 0)))

(define (score lst i j)
  (let ((l (car (drop lst j)))
        (trans-l (car (drop (transpose lst) i))))
    (* (subscore l i)
       (subscore (reverse l) (- (length l) i 1))
       (subscore trans-l j)
       (subscore (reverse trans-l) (- (length trans-l) j 1)))))

(define (max-score lst)
  (let ((max 0))
    (begin
      (for ((j (in-range 1 (- (length lst) 1))))
        (for ((i (in-range 1 (- (length (car lst)) 1))))
          (let ((current (score lst i j)))
            (cond ((> current max) (set! max current))))))
      max)))

(define (part2 input)
  (let ((vinput (lines->matrix2 input)))
    (max-score vinput)))

(time (begin (displayln (part1 input))
             (displayln (part2 input))))
