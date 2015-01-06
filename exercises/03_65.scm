(load "../examples/math.scm")
(load "../examples/streams.scm")

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(display "ln2-stream")
(newline)
(display-stream-upto ln2-stream 10)
(newline)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display "Euler transformed ln2-stream:")
(newline)
(display-stream-upto (euler-transform ln2-stream) 10)
(newline)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display "super-accelerated ln2-stream:")
(newline)
(display-stream-upto (accelerated-sequence euler-transform ln2-stream) 9)
(newline)
