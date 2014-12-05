(use test)

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

(define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(define (gcd a b)
    (if (= b 0)
        (abs a)
        (gcd (abs b) (remainder (abs a) (abs b)))))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (make-rat2 n d)
    (cond ((and (> n 0) (> d 0)) (make-rat n d))
          ((and (< n 0) (< d 0)) (make-rat n (abs d)))
          ((and (< n 0) (> d 0)) (make-rat n d))
          ((and (> n 0) (< d 0)) (make-rat (- n) (abs d)))))

(test "(make-rat2 1 2) = (1 2)" (cons 1 2) (make-rat2 1 2))
(test "(make-rat2 -1 2) = (-1 2)" (cons -1 2) (make-rat2 -1 2))
(test "(make-rat2 1 -2) = (-1 2)" (cons -1 2) (make-rat2 1 -2))
(test "(make-rat2 -1 -2) = (-1 2)" (cons -1 2) (make-rat2 -1 -2))
