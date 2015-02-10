; partial-sum: p_(n+1) = s_(n+1) + s_n

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams partial-sums (stream-cdr s)))
