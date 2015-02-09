(use test)
(use extras)
(load "evalapply.scm")

; Exercise 4.1:
(printf "Exercise 4.1:~N")
; As the metacircular evaluator isn't completely implemented yet,
; I'm doing this exercise by testing just the list-of-values procedure.
; Things we must define in order to make list-of-values work:
(define (eval a b) a)
(define (first-operand l) (car l))
(define (rest-operands l) (cdr l))
(define (no-operands? l) (null? l))

; Original implementation of list-of-values that evaluates left-to-right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; This should return (1 2 3 4)
(test "Evaluating '(1 2 3 4) with original version:" '(1 2 3 4) (list-of-values '(1 2 3 4) 'env))

; Revised implementation that evalutates left-to-right regardless of
; the evaluation order of cons. We acheive this by first calling eval
; on first-operand then recursively calling list-of-values on rest-operands.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
        (cons left right))))

; This should return (1 2 3 4)
(test "Evaluating '(1 2 3 4) with forced left-to-right version:" '(1 2 3 4) (list-of-values '(1 2 3 4) 'env))

; Revised implementation that evalutates right-to-left regardless of
; the evaluation order of cons. We acheive this by recursively calling
; list-of-values on rest-operands before evaluating anything.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env))
            (left (eval (first-operand exps) env)))
        (cons left right))))

; This should return (4 3 2 1)
(test "Evaluating '(1 2 3 4) with forced right-to-left version:" '(4 3 2 1) (list-of-values '(4 3 2 1) 'env))


