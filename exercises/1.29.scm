(use extras)
(use test)

; Simpson's rule

; simpson
(define (simpson f a b n)
  ; h = (b - a) / n
  (define simpson-h (/ (- b a) n))

  ; yk = f(a + kh)
  (define (simpson-y k)
    (f (+ a (* k simpson-h))))

  ; term
  ; y0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n
  (define (simpson-term k)
    (cond ((= k 0) (simpson-y 0))
          ((= k n) (simpson-y n))
          ((= (modulo k 2) 1) (* 4 (simpson-y k)))
          ((= (modulo k 2) 0) (* 2 (simpson-y k)))))

  ; sum
  (define (simpson-sum k n)
    (if (> k n)
        0
        (+ (simpson-term k)
           (simpson-sum (+ k 1) n))))

  ; (h / 3)(y0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n)
  (* (/ simpson-h 3)
     (simpson-sum 0 n)))

; test
; unity
(define (unity x) 1)

; identity
(define (identity x) x)

; cube
(define (cube x) (* x x x))

(define n 10)

(printf "~S~N" (simpson unity 0 1 n))
(printf "~S~N" (simpson identity 0 1 n))
(printf "~S~N" (simpson cube 0 1 n))

(test "10" 0.25 (simpson cube 0 1 10))
(test "9" 0.25 (simpson cube 0 1 9))
(test "7" 0.25 (simpson cube 0 1 7))
(test "5" 0.25 (simpson cube 0 1 5))
