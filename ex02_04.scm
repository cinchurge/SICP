(use test)

; Ex2.4:
; Alternate representation of pairs:
(define (cons-new x y)
  (lambda (m) (m x y)))
; This creates a lambda and stores x and y in its body.
; The argument m should be a lambda that takes two arguments.

(define (car-new z)
  (z (lambda (p q) p)))
; This creates a lambda that takes two arguments, and when
; applied to a "procedural pair", returns the first argument

(define (cdr-new z)
  (z (lambda (p q) q)))
; cdr is just the same as car, except we return the second
; argument instead of the first

(test 1 (car-new (cons-new 1 2)))
(test 2 (cdr-new (cons-new 1 2)))
(test 3 (car-new (cons-new (+ 1 2) 2)))
(test 7 (cdr-new (cons-new (+ 1 2) (+ 3 4))))
