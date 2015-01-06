(load "../examples/streams.scm")

; partial-sum: p_(n+1) = s_(n+1) + p_n

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define sums (partial-sums (integers-starting-from 0)))

(display-stream-upto sums 10)
