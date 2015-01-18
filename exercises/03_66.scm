; SICP exercise 3.66
; (c) 2015 Eric Chung
;
; I found the best way to tackle this problem is to write out
; the term count in a matrix similar to the figure on page 460.

(load "../examples/streams.scm")

(define (pairs s t)
  ;(printf "pairs ~S ~S~N" s t)
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

; Printing out the first 30 terms, this is what it looks like:
;   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16
; 1 1   2   4   6   8  10  12  14  16   18  20  22  24  26  28  30
; 2     3   5   9  13  17  21  25  29
; 3         7  11  19  27
; 4            15  23
; 5
; 6
; 7
; 8
; 9
;10
;
; From first glance, it is apparent that except for the first term,
; all of the even terms are located on the first row, while all of
; the odd terms are distributed in the rest of the upper-triangular
; of the matrix. This matches the behavior of the design of the
; paris procedure in which the first row is interleaved with the second
; row, then the combination is interleaved with the third row, and so on.
; The interleave procedure goes back and forth between the first row
; and the rest of the rows, therefore it is reasonable that the even
; terms are all located on the first row.
;
; Looking more closely at the matrix, you can see that each of the diagonal
; terms form a series with increasing difference between terms. For example,
; The main diagonal terms 1, 3, 7, 15 are separated by 2, 4, 8, each an
; increasing power of 2. The diagonal next to the main diagonal is
; 2, 5, 11, 23, with separations 3, 6, 12, which is 3 multiplied by
; increasing powers of 2. The diagonal after that is 4, 9, 19, with separations
; 5, 10, which is 5 multiplied by increasing powers of 2... and so on.
;
; These relationships can be summarized with the following recursive
; rules:
;
; n(1,1) = 1
; n(i,i) = n(i-1,i-1) + 2^(i - 1) for all i > 1
; n(i,j) = n(i-1,j-1) + (2 * (j - i) + 1) * 2^(i - 1)
;
; In scheme, we can define a procedure that calculates the term
; number from the integer pair:
;
(define (get-pair-n p)
  (let ((r (car p))
        (c (cadr p)))
     (cond ((and (= r 1) (= c 1)) 1) ; base case
           ((= r 1) (* 2 (- c 1))) ; all even numbers are on the first row
           ((= r c) (+ (get-pair-n (list (- r 1) (- c 1))) (expt 2 (- r 1)))) ; main diagonal
           (else ; all other terms
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

; (1 100) is at term 198, meaning there are 197 terms before it.
; (99 100) has on the order of 9.50737950171172e+29 terms before it.
; (100 100) has on the order of 1.26765060022823e+30 terms before it.
