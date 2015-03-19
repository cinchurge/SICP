(use test)
(use extras)
(load "metacircular.scm")

; In order to use set-car! and set-cdr!, the
; frame must looks something like this:
; (() (pair1 pair2 pair3 ...))
(define (make-frame vars vals)
  (define (make-frame-iter vars vals)
    (if (null? vars)
        '()
        (cons (cons (car vars) (car vals)) (make-frame-iter (cdr vars) (cdr vals)))))
  (cons '() (make-frame-iter vars vals)))



; We use (frame-pairs frame) to extract the list of pairs
(define (frame-pairs frame)
  (cdr frame))



; Add a pair to the head of the list of pairs
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (frame-pairs frame))))



; The cool thing about using pairs (or association lists) for
; variable bindings is that we can use assoc with the extended
; cond clauses to simplify variable assignment
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((assoc var (frame-pairs frame)) => (lambda (x) (set-cdr! x val)))
            (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))



; assoc and cond inaction again
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((assoc var (frame-pairs frame)) => cdr)
            (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))



; and again
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((assoc var (frame-pairs frame)) => (lambda (x) (set-cdr! x val)))
            (else (add-binding-to-frame! var val frame))))
    (scan frame)))



; Unit tests
; Make sure the evaluator still works
(printf "Metacircular evaluator unit tests~N")
(load "test-metacircular.scm")
(printf "~N")

(printf "Exercise 4.12 unit tests~N")
; Test variable binding and lookup
(let ((env (setup-environment)))
  (test '(() (a . 1) (b . 2) (c . 3)) (make-frame '(a b c) '(1 2 3)))
  (let ((env2 (extend-environment '() '() env)))
    (define-variable! 'a 0 env2)
    (test 0 (lookup-variable-value 'a env2))
    (define-variable! 'a 1 env2)
    (test 1 (lookup-variable-value 'a env2))
    (define-variable! 'd 4 env2)
    (test 4 (lookup-variable-value 'd env2))
  )
  (let ((env2 (extend-environment '(a b c) '(1 2 3) env)))
    (set-variable-value! 'a 0 env2)
    (test 0 (lookup-variable-value 'a env2))
    (define-variable! 'a 1 env2)
    (test 1 (lookup-variable-value 'a env2))
    (define-variable! 'd 4 env2)
    (test 4 (lookup-variable-value 'd env2))
  )
)
(printf "~N")

