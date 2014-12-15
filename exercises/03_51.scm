(load "../examples/streams.scm")

(define (show x)
    (display-elm x)
    x)

(define xx
    (stream-map show
                (stream-enumerate-interval 0 10)))
(stream-ref xx 5)
(stream-ref xx 7)
