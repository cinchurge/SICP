; Exercise 2.6: define one and two as Church numerals

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; => (lambda (f) (lambda (x) (f ((zero f) x))))
; => (lambda (f) (lambda (x) (f (((lambda (x) x) f) x))))
; => (lambda (f) (lambda (x) (f (f x))))


(display (((add-1 (add-1 zero)) (lambda (x) (+ x 1))) 0))
