(use test)
(use extras)
(load "ex04_06.scm")

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
        ((let*? expr) (micro-eval (let*->nested-lets expr) env))
        ((begin? expr) (eval-sequence (begin-actions expr) env))
        ((cond? expr) (micro-eval (cond->if expr) env))
        ((application? expr)
         (micro-apply (micro-eval (operator expr) env)
                      (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type: EVAL" expr))))

; let*?
(define (let*? expr)
  (tagged-list? expr 'let*))

; let*->nested-lets
(define (let*->nested-lets expr)
  (let ((vars (let-vars expr))
        (body (let-body expr)))
    (define (let*->nested-lets-helper vars)
        (if (null? (cdr vars))
            ; If we see that this is the last var assignment, append body
            ; to the assginment
            (append (list 'let (list (car vars))) body)
            (list 'let (list (car vars)) (let*->nested-lets-helper (cdr vars)))))
    (let*->nested-lets-helper vars)))
        

; Unit tests
(printf "Exercise 4.7~N")
(let ((env (setup-environment))
      (expr '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))))
  (test '((x 3) (y (+ x 2)) (z (+ x y 5))) (let-vars expr))
  (test '((* x z)) (let-body expr))
  (test '(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z)))) (let*->nested-lets expr))
  (micro-eval expr env)
  (test 39 (micro-eval expr env))
)
(printf "~N")
