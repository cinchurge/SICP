(use test)

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
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; Exercise 2.7:
; Implement the constructor and selectors
(define (make-interval l u) (cons l u))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Unit tests
(let ((x (make-interval -0.5 0.5))
      (y (make-interval -1 1)))
  (test 0.5 (upper-bound x))
  (test -0.5 (lower-bound x))
  (let ((a (add-interval x y))
        (m (mul-interval x y))
        (d (div-interval x y)))
    (test 1.5 (upper-bound a))
    (test -1.5 (lower-bound a))
    (test -0.5 (lower-bound m))
    (test 0.5 (upper-bound m))
    (test -0.5 (lower-bound d))
    (test 0.5 (upper-bound d))
  )
)

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

; Exercise 2.11:
; Modify mul-interval to take into account the sign of the
; interval endpoints

(define (mul-interval x y)
  (let ((a (upper-bound x))
        (b (lower-bound x))
        (c (upper-bound y))
        (d (lower-bound y)))
    (cond ((and (> a 0) (> b 0))
           (cond ((and (> c 0) (> d 0)) (make-interval (* a c) (* b d)))
                 ((and (> c 0) (< d 0)) (make-interval (* a c) (* a d)))
                 ((and (< c 0) (< d 0)) (make-interval (* b c) (* b d)))))
          ((and (> a 0) (< b 0))
           (cond ((and (> c 0) (> d 0)) (make-interval (* a c) (* b c)))
                 ((and (> c 0) (< d 0)) (make-interval (max (* a c) (* b d)) (min (* a d) (* b c))))
                 ((and (< c 0) (< d 0)) (make-interval (* b c) (* a d)))))
          ((and (> a 0) (< b 0))
           (cond ((and (> c 0) (> d 0)) (make-interval (* a d) (* b c)))
                 ((and (> c 0) (< d 0)) (make-interval (* b d) (* b c)))
                 ((and (< c 0) (< d 0)) (make-interval (* b d) (* a c))))))))
