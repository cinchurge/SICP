(use extras)
(load "streams.scm")
(load "math.scm")

; Infinite stream of integers
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; Fibonacci generator
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (display-stream-upto s n)
  (define (loop c n)
    (if (> c n)
      (display "")
      (begin
        (printf "~S~N" (stream-ref s c))
        (loop (+ c 1) n))))
  (loop 1 n))

(printf "First 10 terms of infinite fib stream:~N")
(display-stream-upto fibs 10)
(printf "~N")

; Sieve of Eratosthenes
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
            (lambda (x)
              (not (divisible? x (stream-car stream))))
            (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(printf "First 10 terms of infinite prime stream:~N")
(display-stream-upto primes 10)
(printf "~N")

; Alternate implementation of fibs
(define fibs2
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs2) fibs2))))

(printf "First 10 terms of infinite fibs2 stream:~N")
(display-stream-upto fibs2 10)
(printf "~N")

; Recursive definition of primes
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  iter primes)
