(use test)

(define (square x) (* x x))

(test "square 21 = 441" 441 (square 21))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(test "sum-of-suqares 3 4 = 25" 25 (sum-of-squares 3 4))

(define (f a)
    (sum-of-squares (+ a 1) (* a 2)))

(test "f 5 = 136" 136 (f 5))
