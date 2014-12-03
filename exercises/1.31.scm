(use test)
(use extras)

(define (identity x) x)
(define (inc x) (+ x 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) inc b))))

; calculate pi
(define (pi-term k)
  (/ (+ 2 
        (* 2
           (floor (/ (+ k 1) 2))))
     (+ 3
        (* 2
           (floor (/ k 2))))))

(printf "10     terms: ~S~N" (* 4 (product pi-term 0 inc 10)))
(printf "100    terms: ~S~N" (* 4 (product pi-term 0 inc 100)))
(printf "1000   terms: ~S~N" (* 4 (product pi-term 0 inc 1000)))
(printf "10000  terms: ~S~N" (* 4 (product pi-term 0 inc 10000)))

(test "(product identity 1 inc 3) = 6" 6 (product identity 1 inc 3))
(test "(product identity 1 inc 4) = 24" 24 (product identity 1 inc 4))
(test "(product identity 1 inc 5) = 120" 120 (product identity 1 inc 5))
