(load "../examples/math.scm")
(load "../examples/streams.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(display "sqrt-stream:")
(newline)
(display-stream-upto (sqrt-stream 2) 10)
(newline)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display "pi-stream:")
(newline)
(display-stream-upto pi-stream 10)
(newline)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display "Euler transformed pi-stream:")
(newline)
(display-stream-upto (euler-transform pi-stream) 10)
(newline)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display "super-accelerated pi-stream:")
(newline)
(display-stream-upto (accelerated-sequence euler-transform pi-stream) 9)
(newline)

(define (pairs s t)
  (printf "pairs ~S ~S~N" s t)
  (cons-stream
    ; We first take the head of both s and t and pair them together
    ; to create the head of the stream
    (list (stream-car s) (stream-car t))
    ; We then pair the head of s with the rest of t, and interleave
    ; that with the sequence of pairs generated with the rest of s
    ; and the rest of t
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  ;(printf "interleave ~S ~S~N" s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ; We must alternate between s1 and s2
                   ; in order to correctly interleave the
                   ; two streams
                   (interleave s2 (stream-cdr s1)))))

(display-stream-upto (pairs integers integers) 30)

(define (sum-pair p)
  (begin
    (printf "p=~S+~S=~S~N" (car p) (car (cdr p)) (sum p))
    (sum p)))

(define (get-pair-n p)
  (let ((r (car p))
        (c (cadr p)))
     ;(printf "r=~S, c=~S~N" r c)
     (cond ((and (= r 1) (= c 1)) 1) ; base case
           ((= r 1) (* 2 (- c 1))) ; all even numbers are on the first row
           ((= r c) (+ (get-pair-n (list (- r 1) (- c 1))) (expt 2 (- r 1))))
           (else
             (let ((base (+ (* 2 (- c r)) 1))
                   (prev (get-pair-n (list (- r 1) (- c 1)))))
             (+ prev (* base (expt 2 (- r 2)))))))))
(printf "(1 1) -> ~S~N" (get-pair-n (list 1 1)))
(printf "(1 2) -> ~S~N" (get-pair-n (list 1 2)))
(printf "(1 3) -> ~S~N" (get-pair-n (list 1 3)))
(printf "(2 2) -> ~S~N" (get-pair-n (list 2 2)))
(printf "(2 3) -> ~S~N" (get-pair-n (list 2 3)))
(printf "(2 4) -> ~S~N" (get-pair-n (list 2 4)))
(printf "(2 5) -> ~S~N" (get-pair-n (list 2 5)))
(printf "(5 6) -> ~S~N" (get-pair-n (list 5 6)))
(printf "(5 7) -> ~S~N" (get-pair-n (list 5 7)))
(printf "(4 7) -> ~S~N" (get-pair-n (list 4 7)))
(printf "(1 100) -> ~S~N" (get-pair-n (list 1 100)))
(printf "(99 100) -> ~S~N" (get-pair-n (list 99 100)))
(printf "(100 100) -> ~S~N" (get-pair-n (list 100 100)))
