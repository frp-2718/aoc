#lang racket

; Quick and simple implementation of mutable 2d arrays ; (0, 0) = top left

(define (make-array rows columns)
  (build-vector rows (lambda (x) (make-vector columns null))))

(define (array-ref array col row)
  (vector-ref (vector-ref array row) col))

(define (array-set! array col row val)
  (vector-set! (vector-ref array row) col val))

(define (array-height array)
  (vector-length array))

(define (array-width array)
  (if (vector-empty? array) 0 (vector-length (vector-ref array 0))))

; Parse input and build the initial map

(define input (list->vector (file->lines (command-line #:args (filename) filename))))

(define (parse input)
  (vector-map (lambda (s) (vector-map string->direction (list->vector (map string (string->list s))))) input))

(define (string->direction s)
  (cond ((string=? s "#") (list 'WALL))
        ((string=? s ".") '())
        ((string=? s ">") (list 'RIGHT))
        ((string=? s "<") (list 'LEFT))
        ((string=? s "^") (list 'UP))
        ((string=? s "v") (list 'DOWN))))

; Simulation

(define (dup-map map)
  (let ((new-map (make-array (array-height map) (array-width map))))
    (for* ((i (array-width map))
           (j (array-height map)))
      (let ((dir (array-ref map i j)))
        (cond ((equal? dir '(WALL)) (array-set! new-map i j '(WALL))))))
    new-map))

(define (comp-state map)
  (let ((new (dup-map map))
        (width (array-width map))
        (height (array-height map)))
    (for* ((i width)
           (j height))
      (let ((dirs (array-ref map i j)))
        (for ((dir dirs))
          (cond ((equal? dir 'RIGHT)
                 (array-set! new (+ (modulo i (- width 2)) 1) j (cons 'RIGHT (array-ref new (+ (modulo i (- width 2)) 1) j))))
                ((equal? dir 'DOWN)
                 (array-set! new i (+ (modulo j (- height 2)) 1) (cons 'DOWN (array-ref new i (+ (modulo j (- height 2)) 1)))))
                ((equal? dir 'UP)
                 (array-set! new i (comp-coord j height) (cons 'UP (array-ref new i (comp-coord j height)))))
                ((equal? dir 'LEFT)
                 (array-set! new (comp-coord i width) j (cons 'LEFT (array-ref new (comp-coord i width) j))))))))
    new))

(define (comp-coord old size)
  (if (> (- old 1) 0) (- old 1) (- size 2)))

(define (find-door map row)
  (define (fs-aux i)
    (if (null? (array-ref map i row))
        (list i row)
        (fs-aux (+ i 1))))
  (fs-aux 0))

(define (find-start map) (find-door map 0))
(define (find-end map) (find-door map (- (array-height map) 1)))

; Build paths

(define make-coord list)
(define x-coord car)
(define y-coord cadr)

(define (update-positions positions grid)
  (let ((current-pos (hash-keys positions)))
    (for ((pos current-pos))
      (let ((up (make-coord (x-coord pos) (- (y-coord pos) 1)))
            (down (make-coord (x-coord pos) (+ (y-coord pos) 1)))
            (right (make-coord (+ (x-coord pos) 1) (y-coord pos)))
            (left (make-coord (- (x-coord pos) 1) (y-coord pos))))
        (when (and (>= (x-coord left) 0) (null? (array-ref grid (x-coord left) (y-coord left))))
          (hash-set! positions left #t))
        (when (and (< (x-coord right) (array-width grid)) (null? (array-ref grid (x-coord right) (y-coord right))))
          (hash-set! positions right #t))
        (when (and (>= (y-coord up) 0) (null? (array-ref grid (x-coord up) (y-coord up))))
          (hash-set! positions up #t))
        (when (and (< (y-coord down) (array-height grid))(null? (array-ref grid (x-coord down) (y-coord down))))
          (hash-set! positions down #t))
        (when (not (null? (array-ref grid (x-coord pos) (y-coord pos)))) (hash-remove! positions pos))))))

(define (build-positions grid positions end count)
  (begin
    (update-positions positions grid)
    (set! initmap (comp-state grid))
    (if (hash-ref positions end #f) count (build-positions initmap positions end (+ count 1)))))

(define initmap (parse input))

(define (part1)
  (let* ((start (find-start initmap))
         (end (find-end initmap))
         (initpos (make-hash (list (cons start #t)))))
    (build-positions initmap initpos end 0)))

(define (part2)
  (let* ((start (find-start initmap))
         (end (find-end initmap))
         (initpos (make-hash (list (cons start #t)))))
    (set! initmap (parse input))
    (+ (build-positions initmap initpos end 0)
       (+ 1 (build-positions initmap (make-hash (list (cons end #t))) start 0))
       (+ 1 (build-positions initmap (make-hash (list (cons start #t))) end 0)))))

(time (begin (displayln (part1))
             (displayln (part2))))
