(load "../examples/evalapply.scm")

; Exercise 4.1:
(display "Exercise 4.1:")
(newline)

; Things we must define in order to make list-of-values work:
(define (eval a b) (display a))
(define (first-operand l) (car l))
(define (rest-operands l) (cdr l))
(define (no-operands? l) (null? l))

; Original implementation of list-of-values that evaluates left-to-right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; This should print 1234
(display "Original version:")
(newline)
(list-of-values '(1 2 3 4) 'env)
(newline)

; Revised implementation that evalutates left-to-right regardless of
; the evaluation order of cons. We acheive this by first calling eval
; on first-operand then recursively calling list-of-values on rest-operands.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
        (cons left right))))

; This should print 1234
(display "left to right:")
(newline)
(list-of-values '(1 2 3 4) 'env)
(newline)

; Revised implementation that evalutates right-to-left regardless of
; the evaluation order of cons. We acheive this by recursively calling
; list-of-values on rest-operands before evaluating anything.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env))
            (left (eval (first-operand exps) env)))
        (cons left right))))

; This should print 4321
(display "right to left:")
(newline)
(list-of-values '(1 2 3 4) 'env)
(newline)

; Exercise 4.2:
(display "Exercise 4.1:")
(newline)
; a: Louis's plan is flawed because the application? test will match
;    any pair, thus treating EVERYTHING as an application. Assignment
;    in this case will be executed at all. (define x 3) will result
;    in an attempt to apply the 'define' procedure with arguments x and 3.
; b: Simply redefine the application? test as:
(define (application? exp)
  (tagged-list? exp 'call))

; Exercise 4.3:

; Exercise 4.4:

