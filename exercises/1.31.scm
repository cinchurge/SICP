(use test)
(use extras)

(define (identity x) x)
(define (inc x) (+ x 1))

; recursive version of product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) inc b))))

; iterative version of product
(define (product-iter term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter a 1))

; calculate pi
(define (pi-term k)
  (/ (+ 2 
        (* 2
           (floor (/ (+ k 1) 2))))
     (+ 3
        (* 2
           (floor (/ k 2))))))

(printf "Recursive product:~N")
(test "(product identity 1 inc 3) = 6" 6 (product identity 1 inc 3))
(test "(product identity 1 inc 4) = 24" 24 (product identity 1 inc 4))
(test "(product identity 1 inc 5) = 120" 120 (product identity 1 inc 5))
(printf "10     terms: ~S~N" (* 4 (product pi-term 0 inc 10)))
(printf "100    terms: ~S~N" (* 4 (product pi-term 0 inc 100)))
(printf "1000   terms: ~S~N" (* 4 (product pi-term 0 inc 1000)))
(printf "10000  terms: ~S~N" (* 4 (product pi-term 0 inc 10000)))
(printf "(segfaults at 100000 terms)~N")

(printf "Iterative product:~N")
(test "(product-iter identity 1 inc 3) = 6" 6 (product-iter identity 1 inc 3))
(test "(product-iter identity 1 inc 4) = 24" 24 (product-iter identity 1 inc 4))
(test "(product-iter identity 1 inc 5) = 120" 120 (product-iter identity 1 inc 5))
(printf "10      terms: ~S~N" (* 4 (product-iter pi-term 0 inc 10)))
(printf "100     terms: ~S~N" (* 4 (product-iter pi-term 0 inc 100)))
(printf "1000    terms: ~S~N" (* 4 (product-iter pi-term 0 inc 1000)))
(printf "10000   terms: ~S~N" (* 4 (product-iter pi-term 0 inc 10000)))
(printf "100000  terms: ~S~N" (* 4 (product-iter pi-term 0 inc 100000)))
(printf "1000000 terms: ~S~N" (* 4 (product-iter pi-term 0 inc 1000000)))

