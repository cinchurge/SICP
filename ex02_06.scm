(use test)

; Exercise 2.6: define one and two as Church numerals, and
; the add operation.

; Church numerals use the number of applications of a function
; as integral data, so (add-1 n) is adding an additional function
; application to n, one is f(x) = f(x), two is f(x) = f(f(x)),
; addition of n and m is f^n(f^m(x)).

(define zero (lambda (f) (lambda (x) x)))
; => f(x) = x

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;
; Not sure if the math is correct...
;
; => add-1(n) = f(x) = f(n(f)(x)) 
; with zero as f(x) = x:
;   add-1(f) = f(x) = f(x)

; (add-1 zero)
; = (lambda (f) (lambda (x) (f ((zero f) x))))
; = (lambda (f) (lambda (x) (f (f x))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define four (lambda (f) (lambda (x) (f (f (f (f x)))))))


(define (increment n) (+ n 1))

(test 1 ((one increment) 0))
(test 2 ((two increment) 0))

; Since each numeral n applied to f is f applied n times,
; the add operation for n and m is f applied n times (n f)
; applied to f applied m times (m f)
(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(test 3 (((add one two) increment) 0))
(test 7 (((add three four) increment) 0))
