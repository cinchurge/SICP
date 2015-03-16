(use test)

(define (cons-new x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car-new z) (z 0))
(define (cdr-new z) (z 1))

(let ((z (cons-new 1 2)))
  (test 1 (car-new z))
  (test 2 (cdr-new z)))
