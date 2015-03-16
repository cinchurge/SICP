(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Straightforwrad implementation of make-rat
(define make-rat cons)
(define numer car)
(define denom cdr)
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; make-rat with reduced numerator and denominator
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; Exercise 2.1:
; make-rat that handles negative numbers properly
(define (make-rat n d)
  (define (make-rat-helper n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (cond ((and (>= n 0) (>= d 0)) (make-rat-helper n d))
        ((and (>= n 0) (< d 0)) (make-rat-helper (* n -1) (* d -1)))
        ((and (< n 0) (< d 0)) (make-rat-helper (* n -1) (* d -1)))
        ((and (< n 0) (>= d 0)) (make-rat-helper n d))))
