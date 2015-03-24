(define (list-ref* items n)
  (if (= n 0)
      (car items)
      (list-ref* (cdr items) (- n 1))))

(define (length* items)
  (if (null? items)
      0
      (+ 1 (length* (cdr items)))))

; Unit tests
(use test)
(let ((squares (list 1 2 9 16)))
  (test 1 (list-ref* squares 0))
  (test 2 (list-ref* squares 1))
  (test 9 (list-ref* squares 2))
  (test 16 (list-ref* squares 3))
  (test 4 (length* squares)))

; Exercise 2.18
; Define a procedure 'reverse' that takes a list
; as argument and returns a list of the same elements
; in reverse order.

(define (reverse-new l)
  (define (reverse-new-helper l r)
    (if (null? l)
        r
        (reverse-new-helper (cdr l) (cons (car l) r))))
  (reverse-new-helper (cdr l) (cons (car l) '())))

; Unit tests
(let ((l (list 1 2 3 4)))
  (test (list 4 3 2 1) (reverse-new l)))

; Exercise 2.19
; Rewrite the counting change problem so that
; the procedure cc takes as its second argument a list
; of values of the coins to use
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(test 292 (cc 100 us-coins))

; Exercise 2.20:
(define (same-parity . lst)
  (define (parity x)
    (if (even? x)
        1
        0))
  (define (same-parity-helper p l ll)
    (if (null? l)
        (reverse ll)
        (if (= p (parity (car l)))
            (same-parity-helper p (cdr l) (cons (car l) ll))
            (same-parity-helper p (cdr l) ll))))
  (same-parity-helper (parity (car lst)) lst '()))

(test '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))
(test '(2 4 6) (same-parity 2 3 4 5 6 7))
