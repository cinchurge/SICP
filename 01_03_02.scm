; general sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
        (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
             a
             (lambda (x) (+ x 4))
             b ))

(printf "~S~N" (* 8 (pi-sum 1 1000)))

(define (f g) (g 2))

(f f)
