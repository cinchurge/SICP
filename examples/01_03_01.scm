(use extras)

; cube
(define (cube x) (* x x x))

; sum-integers
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; sum-cubes
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

; pi-sum
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; general sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; sum-integers2
(define (identity x) x)
(define (inc n) (+ n 1))
(define (sum-integers2 a b)
  (sum identity a inc b))

; sum-cubes2
(define (sum-cubes2 a b)
  (sum cube a inc b))

; pi-sum2
(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))
(define (pi-sum2 a b)
  (sum pi-term a pi-next b))

; definite integral
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; test code
(printf "(sum-integers 1 10) = ~S~N" (sum-integers 1 10))
(printf "(sum-integers2 1 10) = ~S~N" (sum-integers2 1 10))
(printf "(sum-cubes 1 10) = ~S~N" (sum-cubes 1 10))
(printf "(sum-cubes2 1 10) = ~S~N" (sum-cubes2 1 10))
(printf "8 * pi-sum = ~S~N" (* 8 (pi-sum 1 10000)))
(printf "8 * pi-sum2 = ~S~N" (* 8 (pi-sum2 1 10000)))
(printf "(integral cube 0 1 0.01) = ~S~N" (integral cube 0 1 0.01))
(printf "(integral cube 0 1 0.001) = ~S~N" (integral cube 0 1 0.001))

