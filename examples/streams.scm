; Delay

; Stream construction
; cons-stream is a special form in mit-scheme, but is not a part of chicken
; scheme, so it must be implemented explicitly
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

; the-empty-stream is just the same as the empty list
(define the-empty-stream '())

; stream-null? is just the same as null?
(define (stream-null? stream)
  (null? stream))

; stream-car and stream-cdr
(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

; stream-ref is the stream analog of list-ref
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; stream-map is the stream analog of map
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; Looping over streams
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; Displaying streams
(define (display-stream s)
  (begin
      (printf " ")
      (stream-for-each display-elm s)
      (printf "~N")))

(define (display-elm x) (printf " ~S" x))

; stream-enumerate-interval
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

; stream-filter
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

