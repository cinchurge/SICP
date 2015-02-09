; Unit tests for the metacircular evaluator

(use test)
(load "metacircular.scm")

(define (test-eval-apply eval-fn)
  (let ((env the-empty-environment))
    (begin
      (test "Self-evaluating: numeric" 1 (eval-fn 1 env))
      (test "Self-evaluating: string" "hello, world!" (eval-fn "hello, world!" env))
      (test "Quoted: 'foo" ''foo (eval-fn '''foo env))
      (test "Quoted: 'bar" ''bar (eval-fn '''bar env))
      (let* ((test-env (extend-environment '() '() the-empty-environment))
             (dummy (define-variable! 'foo 42 test-env))
             (result-foo (lookup-variable-value 'foo test-env))
             (dummy (define-variable! 'bar 24 test-env))
             (result-bar (lookup-variable-value 'bar test-env)))
        (test "set/lookup variable foo" 42 result-foo)
        (test "set/lookup variable bar" 24 result-bar))
      (let* ((test-env (extend-environment '() '() the-empty-environment)))
        (define-variable! 'foobar 42 test-env)
        (test "Define variable foobar" 42 (lookup-variable-value 'foobar test-env))
        (test "Eval variable foobar" 42 (eval-fn 'foobar test-env))
        (eval-fn '(set! foobar 24) test-env)
        (test "Assign variable foobar" 24 (eval-fn 'foobar test-env))
        (test "Sequence evaluation"
              42
              (eval-fn '(begin (define xx 42) xx) test-env)))
      (test "If with true predicate" 42 (eval-fn '(if true 42 24) env))
      (test "If with false predicate" 24 (eval-fn '(if false 42 24) env))
      )))



(test-eval-apply micro-eval)

