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

(define (make-rect min-p max-p)
    (cons min-p max-p))

(define (rect-min rect)
    (car rect))

(define (rect-max rect)
    (cdr rect))

(define (rect-height rect)
    (- (y-point (rect-max rect))
       (y-point (rect-min rect))))

(define (rect-width rect)
    (- (x-point (rect-max rect))
       (x-point (rect-min rect))))

(define (rect-perim rect)
    (+ (* (rect-height rect) 2)
       (* (rect-width rect) 2)))

(define (rect-area rect)
    (* (rect-height rect)
       (rect-width rect)))

(let ((min-p (make-point 0 0))
      (max-p (make-point 2 1)))
    (let ((rect (make-rect min-p max-p)))
        (test "(rect-perim rect) = 6" 6 (rect-perim rect))
        (test "(rect-area rect) = 2" 2 (rect-area rect))))

