(use test)
(use extras)

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n)
    (= n (smallest-divisor n)))

; Recursive version
(define (filtered-accumulate combiner null-value term a next b filt?) 0
    (if (> a b)
        null-value
        (if (filt? a)
            (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filt?))
            (filtered-accumulate combiner null-value term (next a) next b filt?))))

; Identiy, inc, sum, product...etc.
(define (identity x) x)

(define (inc x)
    (+ x 1))

(define (sum a b)
    (+ a b))

(define (product a b)
    (* a b))

; Prime numbers between 2 and 10: 2, 3, 5, 7
; sum of said prime numbers: 4+9+25+49=87
(test "prime? 2 = t" #t (prime? 2))
(test "prime? 3 = t" #t (prime? 3))
(test "prime? 4 = f" #f (prime? 4))
(test "prime? 5 = t" #t (prime? 5))
(test "prime? 6 = f" #f (prime? 6))

(printf "Recursive version:~N")
(test "sum of squares of primes between 2..10 = 87" 87 (filtered-accumulate sum 0 square 2 inc 10 prime?))
