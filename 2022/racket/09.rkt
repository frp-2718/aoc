#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (make-coord x y)
  (list x y))

(define (coord-x c) (car c))
(define (coord-y c) (cadr c))

(define (make-rope len)
  (make-vector len (make-coord 0 0)))

(define (parse input)
  (define (parse-cmd cmd)
    (let ((dir (string-ref cmd 0))
          (n (string->number (substring cmd 2))))
      (cond ((eq? dir #\R) (make-coord n 0))
            ((eq? dir #\L) (make-coord (- n) 0))
            ((eq? dir #\U) (make-coord 0 n))
            ((eq? dir #\D) (make-coord 0 (- n))))))
  (map parse-cmd input))

(define (move-head rope motion)
  (vector-set! rope 0 (map + (vector-ref rope 0) motion)))

(define (move-knot prev curr)
  (let ((diff (map (lambda (x y) (abs (- x y))) prev curr)))
    (cond ((and (< (car diff) 2) (< (cadr diff) 2)) curr)
          ((>= (car diff) 2) (make-coord (/ (+ (coord-x prev) (coord-x curr)) 2) (coord-y prev)))
          (else (make-coord (coord-x prev) (/ (+ (coord-y prev) (coord-y curr)) 2))))))

(define pos '())

(define (update-rope rope)
  (for ((i (in-range 1 (vector-length rope))))
    (begin
      (vector-set! rope i (move-knot (vector-ref rope (- i 1)) (vector-ref rope i)))
      (set! pos (cons (vector-ref rope (- (vector-length rope) 1)) pos)))))

(define (move-rope rope motion)
  (let ((base (map (lambda (x) (if (= x 0) x (/ x (abs x)))) motion))
        (rep (if (= (car motion) 0) (abs (cadr motion)) (abs (car motion)))))
    (for ((i rep))
      (begin
        (move-head rope base)
        (update-rope rope)))))

(define (apply-motions rope motions)
  (for ((motion motions))
    (move-rope rope motion)))

(time (begin (define motions (parse input))
             (apply-motions (make-rope 2) motions)
             (displayln (length (remove-duplicates pos)))
             (set! pos '())
             (apply-motions (make-rope 10) motions)
             (displayln (length (remove-duplicates pos)))))
