#lang racket

(define (convert char)
  (cond ((or (equal? char #\X) (equal? char #\A)) 1)
        ((or (equal? char #\Y) (equal? char #\B)) 2)
        ((or (equal? char #\Z) (equal? char #\C)) 3)))

(define input (map (lambda (str) (list (convert (string-ref str 0)) (convert (string-ref str 2)))) (file->lines (command-line #:args (filename) filename))))
               
(define (score strategy f)
  (define (score-aux round total)
    (if (null? round)
        total
        (score-aux (cdr round) (+ total (f (car round))))))
  (score-aux strategy 0))

(define (round-choice a b)
  (cond ((or (= (+ a b) 2) (= (+ a b) 5)) 3)
        ((or (= (+ a b) 3) (= (+ a b) 6)) 1)
        (else 2)))

(define (outcome-round a b)
  (let ((res (- a b)))
    (cond
      ((or (= res -2) (= res 1)) 0)
      ((= res 0) 3)
      (else 6))))

(define (score-round-1 round)
  (let ((move1 (car round)) (move2 (cadr round)))
    (+ move2 (outcome-round move1 move2))))

(define (score-round-2 round)
  (let* ((move1 (car round))
         (res (cadr round))
         (move2 (round-choice move1 res)))
    (+ move2 (outcome-round move1 move2))))

(time (begin
        (displayln (score input score-round-1))
        (displayln (score input score-round-2))))
