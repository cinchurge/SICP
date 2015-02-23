(use test)
(use extras)
(load "ex04_07.scm")

(define (named-let? expr)
  (symbol? (cadr expr)))

(define (named-let-func expr)
  (cadr expr))

;(let func (vars) body)

(define (named-let-vars expr)
  (caddr expr))

(define (named-let-body expr)
  (cdddr expr))

(define (let->combination expr)
  ; Create a lambda from the parameters and apply it to the evaluated expressions
  (if (named-let? expr)
      (let ((func (named-let-func expr))
            (parms (collect-lambda-parms (named-let-vars expr))))
           (printf "func: ~S~N" func)
           (printf "parms: ~S~N" parms)
           '((define (func parms))))
      (cons (make-lambda (collect-lambda-parms (let-vars expr))
                         (let-body expr))
            (collect-lambda-exprs (let-vars expr)))))



; Unit tests
(printf "Exercise 4.8~N")
(let ((env (setup-environment))
      (expr '(let fib-iter ((a 1) (b 0) (count 3)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
      (expr2 '(let ((a (+ 13 24)) (b (+ 57 68))) (+ (* a 9) b)))
      (expr3 '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))))
  (printf "~S~N" (let->combination expr))
  (test 39 (micro-eval expr3 env))
  (test 458 (micro-eval expr2 env))
  (test #t (named-let? expr))
  (test 'fib-iter (named-let-func expr))
  (test '((a 1) (b 0) (count 3)) (named-let-vars expr))
  (test '((if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (named-let-body expr))
)
(printf "~N")
