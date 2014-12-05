(use test)

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(let ((p (make-point 1 2)))
    (test "(x-point p) = 1" 1 (x-point p))
    (test "(y-point p) = 2" 2 (y-point p)))

(define (make-segment sp ep)
    (cons sp ep))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

(let ((sp (make-point 1 2))
      (ep (make-point 3 4)))
    (let ((s (make-segment sp ep)))
        (test "(start-segment s) = (1 2)" sp (start-segment s))
        (test "(end-segment s) = (3 4)" ep (end-segment s))))
