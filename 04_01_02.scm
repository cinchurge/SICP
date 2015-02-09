(use test)
(use extras)
(load "evalapply.scm")

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

