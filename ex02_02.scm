(load "rational.scm")
(use test)
(use extras)

; Exercise 2.2
; Define make-segment: constructor for line segment
;        start-segment: selector for segment start point
;        end-segment: selector for segment end point
;        make-point: constructor for point
;        x-point: selector for x of point
;        y-point: selector for y of point
;        midpoint-segment: procedure to return midpoint of line segment
(define (print-point p)
  (printf "(~S,~S)~N" (x-point p) (y-point p)))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p-start p-end)
  (cons p-start p-end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((p0 (start-segment s))
        (p1 (end-segment s)))
    (make-point (/ (+ (x-point p0) (x-point p1)) 2.0)
                (/ (+ (y-point p0) (y-point p1)) 2.0))))

; Unit tests
(let* ((p0 (make-point 0.0 0.0))
       (p1 (make-point 1.0 1.0))
       (s (make-segment p0 p1))
       (m (midpoint-segment s)))
  (test 0.5 (x-point m))
  (test 0.5 (y-point m))
  (print-point m))
