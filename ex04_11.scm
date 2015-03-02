(use test)
(use extras)
(load "metacircular.scm")

(define (make-frame vars vals)
  (if (or (null? vars) (null? vals))
      '()
      (cons (cons (car vars) (car vals)) (make-frame (cdr vars) (cdr vals)))))



(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((assoc var frame) => (lambda (x) (set-cdr! x val)))
            (else (enclosing-environment env))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))



(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((assoc var frame) => cdr)
            (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))



(define (add-binding-to-frame! var val frame)
  (let ((new-frame (cons (cons var val) (copy frame))))
    (printf "~S~N" new-frame)
    (set-car! frame new-frame))
    (printf "~S~N" frame))



(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((assoc var frame) => (lambda (x) (set-cdr! x val)))
            (else (add-binding-to-frame! var val frame))))
    (scan frame)))



(let ((env (setup-environment)))
  (test '((a . 1) (b . 2) (c . 3)) (make-frame '(a b c) '(1 2 3)))
  (let ((env2 (extend-environment '(a b c) '(1 2 3) env)))
    (set-variable-value! 'a 0 env2)
    (test 0 (lookup-variable-value 'a env2))
    (define-variable! 'a 1 env2)
    (test 1 (lookup-variable-value 'a env2))
    (define-variable! 'd 4 env2)
    (printf "~S~N" env2)
    (test 4 (lookup-variable-value 'd env2))
  )
)


(load "test-metacircular.scm")
