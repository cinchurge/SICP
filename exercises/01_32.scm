(use test)
(use extras)

; Recursive version
(define (accumulate combiner null-value term a next b) 0
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; Iterative version
(define (accumulate-iter combiner null-value term a next b) 0
    (define (iter x result)
        (if (> x b)
            result
            (iter (next x) (combiner (term x) result))))
    (iter a null-value))

; Identiy, inc, sum, product...etc.
(define (identity x) x)

(define (inc x)
    (+ x 1))

(define (sum a b)
    (+ a b))

(define (product a b)
    (* a b))

(printf "Recursive version:~N")
(test "sum 0..10 = 55" 55 (accumulate sum 0 identity 0 inc 10))
(test "sum 0..3 = 6" 6 (accumulate sum 0 identity 0 inc 3))
(test "sum 0..1 = 1" 1 (accumulate sum 0 identity 0 inc 1))

(test "product 0..3 = 6" 6 (accumulate product 1 identity 1 inc 3))
(test "product 0..2 = 6" 2 (accumulate product 1 identity 1 inc 2))
(test "product 0..1 = 1" 1 (accumulate product 1 identity 1 inc 1))

(printf "Iterative version:~N")
(test "sum 0..10 = 55" 55 (accumulate-iter sum 0 identity 0 inc 10))
(test "sum 0..3 = 6" 6 (accumulate-iter sum 0 identity 0 inc 3))
(test "sum 0..1 = 1" 1 (accumulate-iter sum 0 identity 0 inc 1))

(test "product 0..3 = 6" 6 (accumulate-iter product 1 identity 1 inc 3))
(test "product 0..2 = 6" 2 (accumulate-iter product 1 identity 1 inc 2))
(test "product 0..1 = 1" 1 (accumulate-iter product 1 identity 1 inc 1))
