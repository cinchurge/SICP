; Stream construction
; cons-stream is a special form in mit-scheme, but not in chicken
; scheme, so it must be implemented separately.
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

; the-empty-stream
(define the-empty-stream '())

; stream-null?
(define (stream-null? stream)
  (null? stream))

; stream-car and stream-cdr
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; Referencing stream elements
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; Mapping over stream elements
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
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

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

