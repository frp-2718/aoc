#lang racket

(define input (file->string (command-line #:args (filename) filename)))

(define (parse-stacks input)
  (let* ((s (string-split (car (string-split input "\n\n")) "\n"))
         (nstacks (quotient (+ (string-length (car s)) 1) 4))
         (stacks (for/vector ((i nstacks)) '())))
    (begin
      (for* ((line s)
             (i (in-range 1 (string-length (car s)) 4)))
        (let ((current (string-ref line i)))
          (when (char-alphabetic? current)
            (vector-set! stacks (quotient i 4) (cons current (vector-ref stacks (quotient i 4)))))))
      (vector-map reverse stacks))))

(define (parse-commands input)
  (let* ((s (string-split (cadr (string-split input "\n\n")) "\n"))
         (ls (map (lambda (str) (string-split str " ")) s)))
    (map (lambda (line) (filter number? (map string->number line))) ls)))

(define program1 (list (parse-stacks input) (parse-commands input)))
(define program2 (list (parse-stacks input) (parse-commands input)))

(define (execute program)
  (let* ((stacks (car program))
         (instr (cadr program)))
    (begin (for ((i instr))
             (for ((n (first i)))
               (let* ((from (vector-ref stacks (- (second i) 1)))
                      (to (vector-ref stacks (- (third i) 1)))
                      (elem (car from)))
                 (vector-set! stacks (- (second i) 1) (cdr from))
                 (vector-set! stacks (- (third i) 1) (cons elem to)))))
           stacks)))

(define (execute-multi program)
  (let* ((stacks (car program))
         (instr (cadr program)))
    (begin (for ((i instr))
             (let* ((from (vector-ref stacks (- (second i) 1)))
                    (to (vector-ref stacks (- (third i) 1)))
                    (elem (take from (first i))))          
               (vector-set! stacks (- (second i) 1) (drop from (first i)))
               (vector-set! stacks (- (third i) 1) (append elem to))))
           stacks)))

(time (begin
        (for ((stack (execute program1)))
          (display (car stack)))
        (newline)
        (for ((stack (execute-multi program2)))
          (display (car stack)))
        (newline)))
