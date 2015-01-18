; SICP exercise 3.67
; (c) 2015 Eric Chung
;
(load "../examples/streams.scm")

(define (pairs s t)
  (printf "pairs ~S ~S~N" s t)
  (cons-stream
    ; We first take the head of both s and t and pair them together
    ; to create the head of the stream
    (list (stream-car s) (stream-car t))
    ; We then pair the head of s with the rest of t, and interleave
    ; that with the sequence of pairs generated with the rest of s
    ; and the rest of t
    (interleave-3
      ; In its original form, we add only one (x, t) term.
      ; now we want both (x, t) and (t, x), so we must add in a new
      ; stream. If we interleave (x, t) with the result of another
      ; interleave, we'll get duplicate results, therefore we'll
      ; need an interleave procedure that takes 3 streams.
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (stream-map (lambda (x) (list x (stream-car s)))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (printf "interleave ~S ~S~N" s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
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
                                                  (interleave-3 (stream-cdr s1)
                                                                (stream-cdr s2)
                                                                (stream-cdr s3))))))))

(display-stream-upto (pairs integers integers) 20)
