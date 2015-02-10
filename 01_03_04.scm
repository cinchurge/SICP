(use extras)

(define (average x y)
  (/ (+ x y) 2.0))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (square x) (* x x))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))

(define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (square y))))
                 1.0))

(printf "~S~N" (sqrt 4))
(printf "~S~N" (sqrt 9))
(printf "~S~N" (sqrt 25))
(printf "~S~N" (cube-root 8))

(define dx 0.00001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

(printf "~S~N" ((deriv cube) 5))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))


