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

; stream-map
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

; add-streams
(define (add-streams s1 s2)
  (stream-map + s1 s2))

; scale-stream
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

; display-stream-upto
(define (display-stream-upto s n)
  (define (display-stream-upto-helper s c n)
    (if (or (>= c n) (stream-null? s))
      (display "")
      (begin
        (printf "~S => ~S~N" (+ c 1) (stream-car s))
        (display-stream-upto-helper (stream-cdr s) (+ c 1) n))))
  (display-stream-upto-helper s 0 n))

; stream->list-upto
(define (stream->list-upto s n)
  (if (= n 0)
      (cons (stream-car s) '())
      (cons (stream-car s) (stream->list-upto (stream-cdr s) (- n 1)))))

; Infinite stream of integers
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; partial-sum: p_(n+1) = s_(n+1) + p_n

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

; list->stream
(define (list->stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (list->stream (cdr lst)))))
    
