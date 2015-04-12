;;
;; List operations
;;

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

;;
;; Mapping over lists
;;

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(test '(10 20 30 40 50) (scale-list (list 1 2 3 4 5) 10))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(test '(10 2.5 11.6 17) (map abs (list -10 2.5 -11.6 17)))
(test '(1 4 9 16) (map (lambda (x) (* x x)) (list 1 2 3 4)))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(test '(10 20 30 40 50) (scale-list (list 1 2 3 4 5) 10))

; Exercise 2.21:
; Compare two versions of square-list by completing the
; definition.
;

; ver 1:
(define (square-list items)
  (if (null? items)
      '()
      (cons ((lambda (x) (* x x)) (car items)) (square-list (cdr items)))))

(test '(1 4 9 16) (square-list '(1 2 3 4)))

; ver 2:
(define (square-list items)
  (map (lambda (x) (* x x)) items))

(test '(1 4 9 16) (square-list '(1 2 3 4)))

; Exercise 2.22:
; Louis's iterative version of square-list:
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items '()))

; The reason the list is reversed is because we're building
; the result in reverse order -- we're cons'ing the head with
; nil, making it the tail of the resultant list.

; Louis's modified version:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items '()))

; this won't work either because the end-of-list setinel will
; be at the head of the list.

; The correct way to do it:
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items '()))

(test '(1 4 9 16) (square-list '(1 2 3 4)))

; Exercise 2.23: implement for-each
(define (for-each* proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each* proc (cdr items)))))

(for-each* (lambda (x) (newline) (display x)) (list 57 321 88))

;;
;; 2.2.2 Hierarchical Structures
;;

(define x (cons (list 1 2) (list 3 4)))
(printf "~S~N" (length x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(test 0 (count-leaves '()))
(test 1 (count-leaves 1))
(test 1 (count-leaves '(1)))
(test 2 (count-leaves '(1 1)))
(test 3 (count-leaves '(1 1 1)))
(test 3 (count-leaves '((1 1) 1)))
(test 4 (count-leaves '((1 1) 1 1)))

; Exercise 2.24:
; (list 1 (list 2 (list 3 4)))
;
; If printed by the interpreter, we get:
; (1 2 3 4)
;
; The corresponding box-and-pointer structure:
;
; Interpretation of this tree:
; the root pair holds a leaf (1) and a pair, which holds
; another leaf (2) and another pair, which holds 3, 4 and nil

; Exercise 2.25:
; Give combinations of cars and cdrs that will pick 7 from each
; of the following lists:
(test 7 (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
(test 7 (car (car '((7)))))
(test 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))

; Exercise 2.26:
(define x (list 1 2 3))
(define y (list 4 5 6))
; Results of:
; (append x y): (1 2 3 4 5 6)
(test '(1 2 3 4 5 6) (append x y))

; (cons x y): ((1 2 3) 4 5 6)
(test '((1 2 3) 4 5 6) (cons x y))

; (list x y): ((1 2 3) (4 5 6))
(test '((1 2 3) (4 5 6)) (list x y))

; Exercise 2.27
; Implement deep-reverse

(define (deep-reverse items)
  (define (deep-reverse-helper items result)
    (if (null? items)
        result
        (let ((head (car items))
              (tail (cdr items)))
          ; Depending on what the head is, we recursively reverse head if it is a list,
          ; swap the fields if it is a pair, or simply continue the reverse if it is a scalar.
          (cond ((list? head) (deep-reverse-helper tail (cons (deep-reverse head) result)))
                ((pair? head) (deep-reverse-helper tail (cons (cons (cdr head) (car head)) result)))
                (else
                  (deep-reverse-helper tail (cons head result)))))))
  (deep-reverse-helper items '()))


(let ((x (list (list 1 2) (list 3 4)))
      (y (list (cons 1 2) (list 3 4))))
  (test '((1 2) (3 4)) ((lambda () x)))
  (test '((3 4) (1 2)) (reverse x))
  (test '((4 3) (2 1)) (deep-reverse x))
  (test '((4 3) (2 . 1)) (deep-reverse y)))

; Exercise 2.28
; Implement the 'fringe' procedure which returns a list of all the leaves
; in the tree arranged in left-to-right order.

(define (fringe tree)
  (define (fringe-helper tree result)
    (if (null? tree)
        result
        (let ((head (car tree))
              (tail (cdr tree)))
          (cond ((list? head) (fringe-helper tail (append result (fringe head))))
                (else
                  (fringe-helper tail (append result (list head))))))))
  (fringe-helper tree '()))

(let ((x (list (list 1 2) (list 3 4))))
  (test '(1 2 3 4) (fringe x))
  (test '(1 2 3 4 1 2 3 4) (fringe (list x x))))

; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len struct)
  (list len struct))

; a. Write the corresponding selectors left-branch,
; right-branch, branch-length and branch-structure
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b. Define a procedure total-weight that returns
; the total weight of a mobile
(define (mobile? x)
  (list? x))

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(let* ((m0 (make-mobile (make-branch 1 1) (make-branch 1 1)))
       (m1 (make-mobile (make-branch 1 m0) (make-branch 1 m0)))
       (m2 (make-mobile (make-branch 1 m1) (make-branch 1 m1))))
  (test 2 (total-weight m0))
  (test 4 (total-weight m1))
  (test 8 (total-weight m2)))

; c. Design a predicate to test whether a binary mobile is balanced:
; A mobile is said to be balanced if the torque applied by its top-left
; branch is equal to that applied by its top-right branch, and if each
; of the submobiles hanging off its branches is balanced.
(define (balanced-mobile? mobile)
  (if (number? mobile)
      #t ; Single weights are always balanced
      (let ((w-left (total-weight (branch-structure (left-branch mobile))))
            (w-right (total-weight (branch-structure (right-branch mobile))))
            (l-left (branch-length (left-branch mobile)))
            (l-right (branch-length (right-branch mobile))))
        (if (= (* w-left l-left) (* w-right l-right))
            (and (balanced-mobile? (branch-structure (left-branch mobile)))
                 (balanced-mobile? (branch-structure (right-branch mobile))))
            #f))))

(let* ((m0 (make-mobile (make-branch 1 1) (make-branch 1 1)))
       (m1 (make-mobile (make-branch 1 1) (make-branch 1 2)))
       (m2 (make-mobile (make-branch 1 m0) (make-branch 1 m0))))
  (test #t (balanced-mobile? m0))
  (test #f (balanced-mobile? m1))
  (test #t (balanced-mobile? m2)))

; d. Change the representation of mobiles so that the constructors are
(define (make-mobile left right) (cons left right))
(define (make-branch len struct) (cons len struct))

; All we need to do is change cadr to cdr for the procedures
; right-branch and branch-structure
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

(let* ((m0 (make-mobile (make-branch 1 1) (make-branch 1 1)))
       (m1 (make-mobile (make-branch 1 1) (make-branch 1 2)))
       (m2 (make-mobile (make-branch 1 m0) (make-branch 1 m0))))
  (test #t (balanced-mobile? m0))
  (test #f (balanced-mobile? m1))
  (test #t (balanced-mobile? m2)))

;;
;; Mapping over trees
;;
; Without using map
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else
          (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(test '(10 (20 (30 40) 50) (60 70)) (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))

; Using map
(define (scale-tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; Exercise 2.30:
; Define a procedure square-tree analogous to the square-list procedure
; of exercise 2.21

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(test '(1 (4 (9 16) 25) (36 49)) (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; Exercise 2.31:
; Design the procedure tree-map that maps functions over trees
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

(test '(10 (20 (30 40) 50) (60 70)) (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))
(test '(1 (4 (9 16) 25) (36 49)) (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; Exercise 2.32:
; Complete the definiton of the 'subsets' procedure that generates
; a set of all subsets of a given set, and explain why it works.
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))

(test '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) (subsets '(1 2 3)))
