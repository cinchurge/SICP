(use test)

; In the absence of a direct measurement, a component (such as a
; resistor) specified to be at x +/- d can take on any
; value within the [x-d, x+d] range. With this in mind,
; everytime we do mathematical operations on the intervals,
; we must always consider every possible combination
; that may result from the operation.
;
; Consider two components with ranges [p0, p1] and
; [q0, q1] where p1 > p0 and q1 > q0.
;
; Addition:
; p1 > 0, p0 > 0:
;   q1 > 0, q0 > 0: [p0 + q0, p1 + q1]
;   q1 > 0, q0 < 0: [p0 + q0, p1 + q1]
;   q1 < 0, q0 < 0: [p0 + q0, p1 + q1]
; p1 > 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 + q0, p1 + q1]
;   q1 > 0, q0 < 0: [p0 + q0, p1 + q1]
;   q1 < 0, q0 < 0; [p0 + q0, p1 + q1]
; p1 < 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 + q0, p1 + q1]
;   q1 > 0, q0 < 0: [p0 + q0, p1 + q1]
;   q1 < 0, q0 < 0; [p0 + q0, p1 + q1]

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
;
; List all the possible situations taking into account
; the sign of the ranges:
; Subtraction: [p0, p1] - [q0, q1]
; p1 > 0, p0 > 0:
;   q1 > 0, q0 > 0: [p0 - q1, p1 - q0]
;   q1 > 0, q0 < 0: [p0 - q1, p1 - q0]
;   q1 < 0, q0 < 0: [p0 - q1, p1 - q0]
; p1 > 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 - q1, p1 - q0]
;   q1 > 0, q0 < 0: [p0 - q1, p1 - q0]
;   q1 < 0, q0 < 0; [p0 - q1, p1 - q0
; p1 < 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 - q1, p1 - q0]
;   q1 > 0, q0 < 0: [p0 - q1, p1 - q0]
;   q1 < 0, q0 < 0; [p0 - q1, p1 - q0]
;
; What this tells us is that we need to subtract
; the upper bound of y from the lower bound x to get
; the resultant lower bound, and subtract the lower bound
; of y from the uppoer bound of x to get the resultant
; upper bound, which makes sense.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Unit tests
(let* ((x (make-interval -1 1))
       (y (make-interval -0.5 0.5))
       (s (sub-interval x y)))
  (test 1.5 (upper-bound s))
  (test -1.5 (lower-bound s)))

(let* ((x (make-interval 0.5 1))
       (y (make-interval 0.5 1))
       (s (sub-interval x y)))
  (test 0.5 (upper-bound s))
  (test -0.5 (lower-bound s)))

; Exercise 2.9:
; Show that the width of the result of addition and subtraction
; is a function only of the widths of the arguments:
;
; let x = [a-d, a+d]
;     y = [b-d', b+d']
;
; Addition:
; x + y = [(a+b)-d-d', (a+b)+d+d']
; width of x + y = (((a+b)+d+d') - ((a+b)-d-d')) / 2
;                = 2(d+d') / 2
;                = d+d'
;
; Subtraction:
; x - y = [(a-b)-d-d', (a-b)+d+d']
; width of x - y = (((a-b)+d+d') - ((a-b)-d-d')) / 2
;                = 2(d+d') / 2
;                = d+d'
;
; Multiplication:
; p1 = (a-d)*(b-d') = ab-ad'-db-dd'
; p2 = (a-d)*(b+d') = ab+ad'-db-dd'
; p3 = (a+d)*(b-d') = ab-ad'+db-dd'
; p4 = (a+d)*(b+d') = ab+ad'+db+dd'

; Exercise 2.10:
; Modify div-interval so that we error out when divide by zero
(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "Division by zero: DIV-INTERVAL")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

; Exercise 2.11:
; Re-implement mul-interval by breaking it into 
; 9 cases based on the signs of the endpoints
;
; Multiplication: [p0, p1] * [q0, q1]
; p1 > 0, p0 > 0:
;   q1 > 0, q0 > 0: [p0 * q0, p1 * q1]
;   q1 > 0, q0 < 0: [p1 * q1, p1 * q0]
;   q1 < 0, q0 < 0: [p1 * q0, p0 * q1]
; p1 > 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 * q1, p1 * q1] 
;   q1 > 0, q0 < 0: [min(p0 * q1, p1 * q0), max(p1 * q1, p0 * q0)
;   q1 < 0, q0 < 0; [p1 * q0, p0 * q0]
; p1 < 0, p0 < 0:
;   q1 > 0, q0 > 0: [p0 * q1, p1 * q0]
;   q1 > 0, q0 < 0: [p0 * q1, p0 * q0]
;   q1 < 0, q0 < 0; [p1 * q1, p0 * q0]

(define (mul-interval x y)
  (let ((p0 (lower-bound x))
        (p1 (upper-bound x))
        (q0 (lower-bound y))
        (q1 (upper-bound y)))
    (cond ((and (> p1 0) (> p0 0))
           (cond ((and (> q1 0) (> q0 0)) (make-interval (* p0 q0) (* p1 q1)))
                 ((and (> q1 0) (< q0 0)) (make-interval (* p1 q1) (* p1 q0)))
                 ((and (< q1 0) (< q0 0)) (make-interval (* p1 q0) (* p0 q1)))))
          ((and (> p1 0) (< p0 0))
           (cond ((and (> q1 0) (> q0 0)) (make-interval (* p0 q1) (* p1 q1)))
                 ((and (> q1 0) (< q0 0)) (make-interval (min (* p0 q1) (* p1 q0)) (max (* p1 q1) (* p0 q0))))
                 ((and (< q1 0) (< q0 0)) (make-interval (* p1 q0) (* p0 q0)))))
          ((and (< p1 0) (< p0 0))
           (cond ((and (> q1 0) (> q0 0)) (make-interval (* p0 q1) (* p1 q0)))
                 ((and (> q1 0) (< q0 0)) (make-interval (* p0 q1) (* p0 q0)))
                 ((and (< q1 0) (< q0 0)) (make-interval (* p1 q1) (* p0 q0)))))
          (else
            (error "interval invalid: MUL-INTERVAL")))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Exercise 2.12:
; Define a constructor make-center-percent that takes a
; center and a percentage tolerance and produces the desired
; interval.
;
; We simply calculate w from c and p and call
; make-center-percent to do the job.
(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

; Exercise 2.13:
; Show that under the assumption of small percentage
; tolerances there is a simple formula for the approximate
; percentage tolerance of the product of two intervals
; in terms of the tolerances of the factors.

; A = a + d
; B = b + d'
; where
;   d = a * p
;   d' = b * q
; A * B = (a + d) * (b + d')
;       = ab + ad' + bd + dd'
; for small tolerances, we can drop dd':
; A * B = ab + ad' + bd
; % = (A * B - ab) / ab
;   = (ad' + bd) / ab
;   = (abq + bap) / ab
;   = q + p

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (mul-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define (print-interval i)
  (printf "~S +/- ~S~N" (center i) (/ (width i) (center i))))

; Exercise 2.14:
(let* ((r1 (make-center-percent 2 0.1))
       (r2 (make-center-percent 4 0.1))
       (rr1 (par1 r1 r2))
       (rr2 (par2 r1 r2)))
  (printf "A: ")(print-interval r1)
  (printf "B: ")(print-interval r2)
  (printf "par1: ")(print-interval rr1)
  (printf "par2: ")(print-interval rr2)
  (printf "A/A: ")(print-interval (div-interval r1 r1))
  (printf "A/B: ")(print-interval (div-interval r1 r2)))

(let* ((r1 (make-center-percent 2 0.01))
       (r2 (make-center-percent 4 0.01))
       (rr1 (par1 r1 r2))
       (rr2 (par2 r1 r2)))
  (printf "A: ")(print-interval r1)
  (printf "B: ")(print-interval r2)
  (printf "par1: ")(print-interval rr1)
  (printf "par2: ")(print-interval rr2)
  (printf "A/A: ")(print-interval (div-interval r1 r1))
  (printf "A/B: ")(print-interval (div-interval r1 r2)))

; Exercise 2.15:
; As long as each operand has uncertainty, the error bounds will increase
; on every operation, even if we're dividing an interval by itself.
; By contrast, taking the reciprocal does not increase the error bounds
; because the interval one is infinitely accurate. Since par1 increases
; the error bounds 3 times and par2 only does it one time, par2 is a better
; implementation. This is somewhat similar to rounding error in
; floating point arithemetic.

; Exercise 2.16:
; TODO
