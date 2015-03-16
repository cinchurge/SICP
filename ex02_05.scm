(use test)

; Exercise 2.5: represent the pair (a, b) as an integer
; 2**a * 3**b. Define cons, car, and cdr

; Raise a to the bth power by recursively multiplying
; a by b times
(define (raise-power a b)
  (define (raise-power-helper a b n)
    (if (= b 0)
        n
        (raise-power-helper a (- b 1) (* a n))))
  (if (= b 0)
      1
      (raise-power-helper a b 1)))

; cons-new works by multiplying 2**a and 2**b
(define (cons-new a b)
  (* (raise-power 2 a) (raise-power 3 b)))

; find-powers-of works by dividing n with b
; until the remainder is no longer 0
(define (find-powers-of b n)
  (define (find-powers-of-helper b n c)
    (cond ((= n 0) 0)
          ((not (= (remainder n b) 0)) c)
          (else (find-powers-of-helper b (/ n b) (+ c 1)))))
  (find-powers-of-helper b n 0))

; car-new works by finding the powers of 2
(define (car-new p)
  (find-powers-of 2 p))

; cdr-new works by finding the powers of 3
(define (cdr-new p)
  (find-powers-of 3 p))

; Unit tests
(test 2 (raise-power 2 1))
(test 4 (raise-power 2 2))
(test 8 (raise-power 2 3))
(test 3 (raise-power 3 1))
(test 9 (raise-power 3 2))
(test 27 (raise-power 3 3))

(test 0 (find-powers-of 2 0))
(test 1 (find-powers-of 2 2))
(test 2 (find-powers-of 2 4))
(test 0 (find-powers-of 3 0))
(test 1 (find-powers-of 3 3))
(test 2 (find-powers-of 3 9))

(let ((p (cons-new 1 2)))
  (test 1 (car-new p))
  (test 2 (cdr-new p)))

(let ((p (cons-new 3 4)))
  (test 3 (car-new p))
  (test 4 (cdr-new p)))
