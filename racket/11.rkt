#lang racket

(define input (file->string (command-line #:args (filename) filename)))

(define (parse-monkey monkey)
  (let ((instr (string-split monkey "\n")))
    (vector (decode-items (second instr))
            (decode-operation (third instr))
            (decode-test (fourth instr) (fifth instr) (sixth instr))
            0)))

(define (decode-items s)
  (map string->number (string-split (substring s 18) ", ")))

(define (decode-operation s)
  (let ((op (string-split (substring s 19) " ")))
    (lambda (x) ((if (string=? (second op) "*") * +)
                 x
                 (if (string=? (third op) "old") x (string->number (third op)))))))

(define (decode-test test iftrue iffalse)
  (let ((divisor (string->number (last (string-split test " "))))
        (monkey-if-true (string->number (last (string-split iftrue " "))))
        (monkey-if-false (string->number (last (string-split iffalse " ")))))
  (list (lambda (x) (if (= 0 (modulo x divisor)) monkey-if-true monkey-if-false)) divisor)))

(define (parse-all in)
  (let ((monkeys (string-split input "\n\n")))
    (list->vector (map parse-monkey monkeys))))

(define (inspect-items monkey monkeys worry-f)
  (let ((items (vector-ref monkey 0))
        (op (vector-ref monkey 1))
        (test (car (vector-ref monkey 2)))
        (divisor (find-div monkeys)))
    (for ((i items))
      (let ((w (worry-f i op divisor)))
        (begin
          (add-item! monkeys w (test w))
          (vector-set! monkey 0 '())
          (vector-set! monkey 3 (+ 1 (vector-ref monkey 3))))))))

(define (add-item! monkeys item m-index)
  (let* ((monkey (vector-ref monkeys m-index))
         (items (reverse (vector-ref monkey 0))))
    (vector-set! monkey 0 (reverse (cons item items)))))

(define (worry-1 item op div)
  (round (/ (- (op item) 1) 3)))

(define (worry-2 item op div)
  (modulo (op item) div))

(define (find-div ms)
  (apply *
         (for/list ((m ms))
           (cadr (vector-ref m 2)))))

(define (play-round vec worry-f)
  (for ((m vec))
    (inspect-items m vec worry-f)))

(define (active-score ms)
  (let* ((sorted-ms (vector-sort ms (lambda (a b) (> (vector-ref a 3) (vector-ref b 3)))))
         (active1 (vector-ref sorted-ms 0))
         (active2 (vector-ref sorted-ms 1)))
    (* (vector-ref active1 3) (vector-ref active2 3))))

(define (part1 worry)
  (let ((ms (parse-all input)))
    (begin
      (for ((i 20))
        (play-round ms worry))
      (active-score ms))))

(define (part2 worry)
  (let ((ms (parse-all input)))
    (begin
      (for ((i 10000))
        (play-round ms worry))
      (active-score ms))))

(time (begin (displayln (part1 worry-1))
             (displayln (part2 worry-2))))
