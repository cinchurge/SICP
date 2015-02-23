(use test)
(use extras)
(load "metacircular.scm")



; micro-eval
(define (micro-eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((true? expr) #t)
        ((false? expr) #f)
        ((variable? expr) (lookup-variable-value expr env))
        ((and? expr) (eval-and expr env))
        ((or? expr) (eval-or expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure (lambda-parameters expr)
                                        (lambda-body expr)
                                        env))
        ((let? expr) (micro-eval (let->combination expr) env))
        ((begin? expr) (eval-sequence (begin-actions expr) env))
        ((cond? expr) (micro-eval (cond->if expr) env))
        ((application? expr)
         (micro-apply (micro-eval (operator expr) env)
                      (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type: EVAL" expr))))



(define (let? expr)
  (tagged-list? expr 'let))

(define (let->combination expr)
  ; Create a lambda from the parameters and apply it to the evaluated expressions
  (cons (make-lambda (collect-lambda-parms (let-vars expr))
                     (let-body expr))
        (collect-lambda-exprs (let-vars expr))))

; The list of local variables/expressions is the cadr of the let expression
(define (let-vars expr) (cadr expr))

; The body is the cddr of the let expression
(define (let-body expr) (cddr expr))

; Collect the parameters to the lambda from the list of local vars
(define (collect-lambda-parms vars)
  (if (null? vars)
      vars
      (cons (caar vars) (collect-lambda-parms (cdr vars)))))

; Collect the expressions to apply to the lambda from the list of local vars
(define (collect-lambda-exprs vars)
  (if (null? vars)
      vars
      (cons (cadar vars) (collect-lambda-exprs (cdr vars)))))



; Unit tests
(printf "Exercise 4.6~N")
(let ((env (setup-environment))
      (expr '(let ((a (+ 10 1)) (b (+ 20 2))) (+ 1 1) (+ (* a 2) b)))
      (expr2 '(let ((a (+ 13 24)) (b (+ 57 68))) (+ (* a 9) b))))
  (test '((a (+ 10 1)) (b (+ 20 2))) (let-vars expr))
  (test '((+ 1 1) (+ (* a 2) b)) (let-body expr))
  (test '(a b) (collect-lambda-parms (let-vars expr)))
  (test '((+ 10 1) (+ 20 2)) (collect-lambda-exprs (let-vars expr)))
  (display '((lambda (a b) (+ (* a 2) b)) (+ 10 1) (+ 20 2)))(newline)
  (test '((lambda (a b) (+ 1 1) (+ (* a 2) b)) (+ 10 1) (+ 20 2)) (let->combination expr))
  (test 44 (micro-eval expr env))
  (test 458 (micro-eval expr2 env))
)
(printf "~N")
