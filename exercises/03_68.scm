; SICP exercise 3.68
; (c) 2015 Eric Chung
;
(load "../examples/streams.scm")

(define (pairs s t)
  (printf "pairs ~S ~S~N" s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

; Louis Reasoner wrote his version of pairs without the cons-stream, which
; will cause pairs to be caught in an infinite recursion.

(define (interleave s1 s2)
  (printf "interleave ~S ~S~N" s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(display-stream-upto (pairs integers integers) 20)
