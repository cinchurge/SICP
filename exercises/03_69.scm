; SICP exercise 3.69
; (c) 2015 Eric Chung
;

(load "../examples/streams.scm")

(define (triples s t u)
  (printf "pairs ~S ~S~N" s t)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave-3
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (interleave s1 s2)
  (printf "interleave ~S ~S~N" s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ; We must alternate between s1 and s2
                   ; in order to correctly interleave the
                   ; two streams
                   (interleave s2 (stream-cdr s1)))))

(define (interleave-3 s1 s2 s3)
  (printf "interleave ~S ~S ~S~N" s1 s2 s3)
  (cond ((stream-null? s1) (interleave s2 s3))
        ((stream-null? s2) (interleave s1 s3))
        ((stream-null? s3) (interleave s1 s2))
        (else
          (cons-stream (stream-car s1)
                       (cons-stream (stream-car s2)
                                     (cons-stream (stream-car s3)

(display-stream-upto (pairs integers integers) 30)

