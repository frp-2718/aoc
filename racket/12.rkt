#lang racket

(define input (file->lines (command-line #:args (filename) filename)))

(define (char->number c)
  (cond ((equal? c #\S) -1)
        ((equal? c #\E) -2)
        (else (- (char->integer c) (char->integer #\a)))))

(struct node (height visited dist) #:mutable)

(define (parse-input input)
  (list->vector (map (lambda (s) (list->vector (map (lambda (c) (node (char->number c) #f +inf.0)) (string->list s))))
                     input)))

(define (find-point grid val)
  (let* ((vec (vector-map (lambda (v) (vector-member val (vector-map node-height v))) grid))
         (ind (for/list ((e vec)
                         (i (vector-length vec)))
                (list i e))))
    (car (filter (lambda (p) (cadr p)) ind))))

(define (find-start grid) (find-point grid -1))

(define (find-end grid) (find-point grid -2))

(define (grid-ref grid coord)
  (vector-ref (vector-ref grid (car coord)) (cadr coord)))

(define (new-dist current new)
  (let ((try-dist (+ (node-dist current) 1)))
    (if (< try-dist (node-dist new))
        try-dist
        (node-dist new))))

(define (dijkstra grid start end rules zerotest)
  (define (dij-aux to-visit)
    (let ((zero (if zerotest (zero-height? to-visit grid) #f)))
      (cond (zero (node-dist zero))
            ((member end to-visit) (node-dist (grid-ref grid end)))
            (else (begin
                    (set-node-visited! (grid-ref grid (car to-visit)) #t)
                    (dij-aux (cdr (step (step (step (step to-visit grid '(0 1) rules) grid '(1 0) rules) grid '(-1 0) rules) grid '(0 -1) rules))))))))
    (dij-aux (list start)))

(define (zero-height? nodes grid)
  (cond ((null? nodes) #f)
        ((<= (node-height (grid-ref grid (car nodes))) 0) (grid-ref grid (car nodes)))
        (else (zero-height? (cdr nodes) grid))))

(define (uniq-append elem l)
  (remove-duplicates (append l (list elem))))

(define (step nodes grid offset rules)
  (let* ((current (car nodes))
         (current-node (grid-ref grid current))
         (next-node (pick (map + current offset) grid)))
    (if (and next-node (rules (node-height next-node) (node-height current-node)))
        (begin
          (set-node-dist! next-node (new-dist current-node next-node))
          (uniq-append (map + current offset) nodes))
        nodes)))

(define (pick coord grid)
  (if (or (< (car coord) 0)
          (> (car coord) (- (vector-length grid) 1))
          (< (cadr coord) 0)
          (> (cadr coord) (- (vector-length (vector-ref grid 0)) 1))
          (node-visited (grid-ref grid coord)))
      #f
      (grid-ref grid coord)))

(define (part1)
  (let* ((grid (parse-input input))
         (start (find-start grid))
         (end (find-end grid)))
    (set-node-dist! (grid-ref grid (find-start grid)) 0)
    (set-node-height! (grid-ref grid (find-end grid)) (- (char->integer #\z) (char->integer #\a)))
    (dijkstra grid start end rule1 #f)))

(define (rule1 dest src)
  (<= (- dest src) 1))

(define (rule2 dest src)
  (<= (- src dest) 1))

(define (part2)
  (let* ((grid (parse-input input))
         (end (find-start grid))
         (start (find-end grid)))
    (set-node-dist! (grid-ref grid start) 0)
    (set-node-height! (grid-ref grid start) (- (char->integer #\z) (char->integer #\a)))
    (dijkstra grid start end rule2 #t)))

(time (begin (displayln (part1))
             (displayln (part2))))
