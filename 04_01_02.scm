(use test)
(use extras)
(load "metacircular.scm")

; Exercise 4.2:
(printf "Exercise 4.2: see comments~N")
(newline)
; a: Louis's plan is flawed because the application? test will match
;    any pair, thus treating EVERYTHING as an application. Assignment
;    in this case will be executed at all. (define x 3) will result
;    in an attempt to apply the 'define' procedure with arguments x and 3.
; b: Simply redefine the application? test as:
(define (application? exp)
  (tagged-list? exp 'call))

; Exercise 4.3:

; micro-eval (data-directed)
(define (test-eval-apply eval-fn)
  (let ((env '()))
    (begin
      (test "Self-evaluating: numeric" 1 (eval-fn 1 env))
      (test "Self-evaluating: string" "hello, world!" (eval-fn "hello, world!" env))
      ;(define foo 42)
      ;(define bar 24)
      ;(test "Variable: foo" 'foo (eval-fn 'foo env))
      ;(test "Variable: bar" 'bar (eval-fn 'bar env))
      (test "Quoted: 'foo" ''foo (eval-fn '''foo env))
      (test "Quoted: 'bar" ''bar (eval-fn '''bar env))
      (test "Assignment: 'bar" ''bar (eval-fn '''bar env))
      )))
(test-eval-apply micro-eval)
; Exercise 4.4:

; Exercise 4.5:
; micro-eval with modified cond
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
        ((cond? expr) (micro-eval (cond->if expr env) env))
        ((application? expr)
         (micro-apply (micro-eval (operator expr) env)
                      (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type: EVAL" expr))))

(let ((env (setup-environment))
      (expr '(cond ((cons 1 2) => car))))
  (test '(b 2) (micro-eval '(assoc  'b '((a 1) (b 2))) env)))

(define (cond->if expr env) (expand-clauses (cond-clauses expr) env))

(define (expand-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((first (car clauses)) ; Let's work on the first clause
            (rest (cdr clauses)))
        (display "expand-clauses")
        (cond ((cond-else-clause? first) ; What to do if we see an else clause
               (if (null? rest) ; Nothing after else, doh!
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF" clauses)))
              ((cond-testapply-clause? first) ; What to do when we see the test => recipient syntax
               (let* ((test (cond-testapply-test first))
                      (test-result (micro-eval test env))
                      (recipient (micro-eval (cond-testapply-recipient first) env)))
                      (make-if test-result
                               (micro-apply recipient (list test-result))
                               (expand-clauses rest env))))
              (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest env)))))))

(define (cond-testapply-clause? clause)
  (and (= (length clause) 3)
       (eq? (cadr clause) '=>)))

(define (cond-testapply-recipient clause)
  (caddr clause))

(define (cond-testapply-test clause)
  (car clause))

;(load "test-metacircular.scm")

(let ((env (setup-environment))
      (expr '(cond ((cons 1 2) => car))))
  (display (micro-eval '(assoc  'b '((a 1) (b 2))) env))(newline)
  (test "(assoc  'b '((a 1) (b 2)))" '(b 2) (micro-eval '(assoc  'b '((a 1) (b 2)))))
  (test "cond-testapply-clause?" #t (cond-testapply-clause? expr))
  (micro-eval '(cond ('(1 2) => car)) env)
  (test "cond: test => recipient" (expand-clauses '(((cons 1 2) => car)) env)))
