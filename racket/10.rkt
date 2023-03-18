#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (parse input)
  (define (cmd line)
    (let ((instr (substring line 0 4)))
      (cond ((string=? instr "noop") (list 0))
            ((string=? instr "addx") (list 0 (string->number (substring line 5)))))))
  (append-map cmd input))

(define (execute prog)
  (define (exec-aux prog ncycles register res)
    (cond ((null? prog) res)
          (else (exec-aux (cdr prog) (+ ncycles 1) (+ register (car prog))
                          (if (member ncycles '(20 60 100 140 180 220))
                              (+ res (* register ncycles))
                              res)))))
  (exec-aux prog 1 1 0))

(define (draw prog)
  (define (draw-aux prog ncycles register res)
    (cond ((null? prog) res)
          (else (draw-aux (cdr prog) (+ ncycles 1) (+ register (car prog))
                          (if (and (>= (modulo ncycles 40) (- register 1)) (<= (modulo ncycles 40) (+ register 1)))
                              (cons ncycles res)
                              (cons #f res))))))
  (draw-aux prog 0 1 '()))

(define (crt pattern)
  (for ((i 241)
        (pixel pattern))
    (begin
      (if pixel (display "#") (display "."))
      (cond ((= (modulo (+ i 1) 40) 0) (displayln ""))))))

(time (begin (displayln (execute (parse input)))
             (crt (reverse (draw (parse input))))))