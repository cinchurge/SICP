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
        ((begin? expr) (eval-sequence (begin-actions expr) env))
        ((cond? expr) (eval-cond expr env))
        ((application? expr)
         (micro-apply (micro-eval (operator expr) env)
                      (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type: EVAL" expr))))

; eval-cond: evaluate all cond expressions
(define (eval-cond expr env)
  (micro-eval (cond->if expr env) env))

; Convert cond to a chain of if expressions
(define (cond->if expr env)
  (expand-clauses (cond-clauses expr) env))

; Expand all cond clauses to a chain of if expressions
(define (expand-clauses clauses env)
  ; Recursively work on the next clause until there are no more clauses
  (if (null? clauses)
      'false
      (let ((first (car clauses)) 
            (rest (cdr clauses)))
        ; If we've hit an else clause, check if it is the last
        ; clause.
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF"
                          clauses)))
              ((cond-extended-clause? first)
               (let ((test (cond-extended-test first))
                     (recipient (cond-extended-recipient first)))
                 (make-if test
                          (list recipient test)
                          (expand-clauses rest env))))
              (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest env)))
        )
      )))

; Test whether a cond clause uses the (test => recipient) syntax:
; (cadr clause) should be =>
(define (cond-extended-clause? clause)
  (eq? (cadr clause) '=>))

(define (cond-extended-test clause)
  (car clause))

(define (cond-extended-recipient clause)
  (caddr clause))

(let ((env (setup-environment)))
  (test 1 (micro-eval '(cond (true 1) (false 2) (else 3)) env))
  (test '(b 2) (micro-eval '(assoc  'b '((a 1) (b 2))) env))
  (test 2 (micro-eval '(cond (false 1) ((assoc  'b '((a 1) (b 2))) => cadr) (else 3)) env))
  (test 3 (micro-eval '(cond (false 1) ((assoc  'c '((a 1) (b 2))) => cadr) (else 3)) env)))

