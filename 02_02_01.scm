(use extras)

(define (list-ref items n)
  (begin
    (printf "n=~S, items=~S~N" n items)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1)))))

(define squares (list 1 4 9 16 25))

(printf "~S~N" (list-ref squares 3))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define x (cons (list 1 2) (list 3 4)))
(printf "length x = ~S~N" (length x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(printf "count-leaves x = ~S~N" (count-leaves x))
