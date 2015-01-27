(load "../examples/streams.scm")

; Python extensions for plotting
(require-extension pyffi)
(py-start)
(py-import "pylab")
(define-pyfun "pylab.plot" y)
(define-pyfun "pylab.semilogx" x y)
(define-pyfun "pylab.xscale" t)
(define-pyfun "pylab.show")

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

; The constant voltage stream
(define (const-voltage v0)
  (cons-stream v0
               (const-voltage v0)))

(define (RC-circuit R C V dt)
  (define v (integral (delay i) 0 dt))
  (define i (stream-map (lambda (x) (/ (- V x) R)) v))
  v)

(pylab.plot (stream->list-upto (RC-circuit 5.0 0.001 10.0 0.05) 1000))
(pylab.show)

; The RC circuit modeled with a constant voltage source
; must be implemented with feedback loops
(define (RC-circuit R C v-input dt)
  (define v (integral (delay i) 0.0 dt))
  (define i (scale-stream (add-streams v-input (scale-stream v -1.0)) (/ 1.0 R)))
  v)

; gen-sine-wave: generate a stream representing a sine wave with
; frequency f, time interval dt
(define (gen-sine-wave a f dt n)
  (cons-stream (* a (sin (* 2.0 3.14159 f (* n dt))))
               (gen-sine-wave a f dt (+ n 1))))

(define (list-max s s0)
  (if (null? s)
      s0
      (if (> (car s) s0)
          (list-max (cdr s) (car s))
          (list-max (cdr s) s0))))

(define R 1.0) ; Ohms
(define C 0.000001) ; Farads
(define V 10.0) ; Volts
(define dt 0.0001)
(define N 10000)
(define freqs (list 1.0 5.0 10.0 50.0 100.0 500.0))
(define amps (map (lambda (f) (list-max (stream->list-upto (RC-circuit R C (gen-sine-wave V f dt 0) dt) N) 0)) freqs))
(pylab.semilogx freqs amps)

(define R 5.0)
(define amps (map (lambda (f) (list-max (stream->list-upto (RC-circuit R C (gen-sine-wave V f dt 0) dt) N) 0)) freqs))
(pylab.semilogx freqs amps)

(define R 10.0)
(define amps (map (lambda (f) (list-max (stream->list-upto (RC-circuit R C (gen-sine-wave V f dt 0) dt) N) 0)) freqs))
(pylab.semilogx freqs amps)

;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 1.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 5.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 10.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 50.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 100.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 500.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 1000.0 0.0001 0) 0.0001) 1000))
;(pylab.plot (stream->list-upto (RC-circuit 100000.0 0.001 (gen-sine-wave 10.0 10000.0 0.0001 0) 0.0001) 1000))
(pylab.show)

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



;
; Exercise 3.77
;
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



;
; Exercise 3.78
;
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-add (stream-scale dy a)
                          (stream-scale y b)))
  y)

(display (stream->list-upto integers 10))
(newline)

; Remember to clean up shop
(py-stop)

