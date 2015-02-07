(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (make-from-real-imag x y) (cons x y))
(define rect "rect")
(put! 'real-part 'rect real-part)

(define x (make-from-real-imag 2 4))

(display ((get 'real-part 'rect) x))
