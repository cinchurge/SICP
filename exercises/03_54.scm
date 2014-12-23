(load "../examples/streams.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; (n+1)! = (n+1) * n!

(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

(display-stream-upto factorials 5)
