(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound x))
                  (/ 1.0 (lower-bound y)))))

; Exercise 2.7:
; Implement the constructor and selectors
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

; Exercise 2.8:
; Describe how the difference of two intervals
; may be computed and the corresponsding procedure
; sub-interval

; The difference d = b - a between two intervals a & b calculated
; should be defined so that a + d = b
(define (sub-interval a b)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Exercise 2.9:

; Exercise 2.10:
; Modify div-interval so that we don't divide by zero
(define (div-interval x y)
  (if (or (= upper-bound 0) (= lower-bound 0))
      (error "Division by zero: DIV-INTERVAL")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound x))
                      (/ 1.0 (lower-bound y))))))

