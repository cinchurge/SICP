(load "../examples/streams.scm")

(define s (cons-stream 1 (add-streams s s)))

(display-stream-upto s 10)
