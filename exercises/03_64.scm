(load "../examples/03_05_03.scm")

(define (stream-limit s t)
  (let ((a0 (stream-car s))
        (a1 (stream-car (stream-cdr s))))
    (if (< (abs (- a1 a0)) t)
        a1
        (stream-limit (stream-cdr s) t))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display "sqrt 2 with tolerance=1.0e-6:")
(newline)
(display (sqrt 2 0.000001))
(newline)
