(use test)

(define (abs x)
    (cond ((> x 0) x)
          ((= x 0) 0)
          ((< x 0) (- x))))

(test "abs -10 = 10" 10 (abs -10))

(define (abs2 x)
    (cond ((< x 0) (- x))
          (else x)))

(test "abs2 -10 = 10" 10 (abs2 -10))

(define (abs3 x)
    (if (< x 0)
        (- x)
        x))

(test "abs3 -10 = 10" 10 (abs3 -10))
