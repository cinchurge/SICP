(load "../examples/streams.scm")

; The integral procedure from the previous section
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; Solution of dy/dt = f(y):
;          => dy = f(y) dt
;          => y = (integral f(y) y0 dt)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

; The above can only work with a delayed integrand:
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; In the above, what's being delayed is the (stream-map f y)
; operation. Forcing it generates a stream that is the result of
; mapping f to y.

; Testcase is dy/dt = y
;          => dy = y dt
;          => (1/y)dy = dt
;          => ln(y) + K = t
;          => y + K' = exp(t)
; since y(0) = 1, we have
;             1 + K' = 1
;          => K' = 0
;          => y = exp(t)
;          => y(1) = e ~ 2.718...
;
; we must stop at t=1, so we set dt=0.001
; and stream-ref the 1000th term:

(let ((ans (solve (lambda (y) y) 1 0.001)))
  (printf "Example: ans=~S~N" (stream-ref ans 1000)))

; Exercise 3.77
; Alternate definition of integral:
(define (integral integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? integrand)
       the-empty-stream
       (integral (stream-cdr integrand)
                 (+ (* dt (stream-car integrand))
                    initial-value)
                 dt))))

; Fixed version of the above:
(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand))) ; We force the delayed-integrand to get the updated integrand
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand)); Since integral expects a delayed integrand, we must delay it here
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

(let ((ans (solve (lambda (y) y) 1 0.001)))
  (printf "Exercise 3.77: ans=~S~N" (stream-ref ans 1000)))
