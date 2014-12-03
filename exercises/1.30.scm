(use test)

(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ a result))))
  (iter a 0))

(test "(sum identity 0 inc 1) = 1" 1 (sum identity 0 inc 1))
(test "(sum identity 0 inc 2) = 3" 3 (sum identity 0 inc 2))
(test "(sum identity 0 inc 3) = 6" 6 (sum identity 0 inc 3))
(test "(sum identity 0 inc 10) = 55" 55 (sum identity 0 inc 10))
