(use extras)

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
        ((cond? expr) (micro-eval (cond->if expr) env))
        ((application? expr)
         (micro-apply (micro-eval (operator expr) env)
                      (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type: EVAL" expr))))

; micro-apply
(define (micro-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: APPLY" procedure))))

; Procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (micro-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Compound procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Conditionals
(define (eval-if expr env)
  (if (micro-eval (if-predicate expr) env)
      (micro-eval (if-consequent expr) env)
      (micro-eval (if-alternative expr) env)))

; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (micro-eval (first-exp exps) env))
        (else
         (micro-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; Assignments
(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (micro-eval (assignment-value expr) env)
                       env)
  'ok)

; Definitions
(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (micro-eval (definition-value expr) env)
                    env)
  'ok)

; Representing expressions
; primitives
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

; variables
(define (variable? expr) (symbol? expr))

; quotes
(define (quoted? expr) (tagged-list? expr 'quote))
(define (text-of-quotation expr) (cadr expr))
(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      #f))

; assignments
(define (assignment? expr) (tagged-list? expr 'set!))
(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))

; definitions
(define (definition? expr) (tagged-list? expr 'define))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))
(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr)
                   (cddr expr))))

; lambdas
(define (lambda? expr) (tagged-list? expr 'lambda))
(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; conditionals
(define (if? expr) (tagged-list? expr 'if))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? expr) (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; procedure application
(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? expr) (tagged-list? expr 'cond))
(define (cond-clauses expr) (cdr expr))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if expr) (expand-clauses (cond-clauses expr)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; Testing of predicates
(define (true? x)
  (eq? x 'true))

(define (false? x)
  (eq? x 'false))

; Environment
; The main APIs are:
; (lookup-variable-value <var> <env>)
; (extend-environment <variables> <values> <base-env>)
; (define-variable! <var> <value> <env>)
; (set-variable-value! <var> <value> <env>)

; The environment is a list of frames, where
; each frame is a table of bindings that associate
; variables with their corresponding values. The
; empty environment is just an empty list.
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment is represented as a pair
; of lists: a list of the variables bound in that frame
; and a list of the associated values.
(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; Extend the environment by creating a new frame consisting
; of the list of variables and the list of values, and we
; adjoin this to the environment.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too fiew arguments supplied" vars vals))))

; Lookup a variable in an environment by scanning the list
; of variables in the first frame. If we find the desired
; variable, return the corresponding element in the list of
; values. If we do not fnd the variable in the current frame,
; we search the enclosing environment, and so on. If we reach
; the empty environment, we signal an "unbound variable" error.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Set a variable to a new value in a specified environment
; by scanning for the variable then changing the corresponding
; value.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; and & or
(define (and? expr) (tagged-list? expr 'and))
(define (eval-and expr env)
  (define (eval-and-helper expr env)
    (let ((result (micro-eval (car expr) env))
          (rest-expr (cdr expr)))
      (if result
          (if (null? rest-expr)
              result
              (eval-and-helper rest-expr env))
          result)))
  (eval-and-helper (cdr expr) env))

(define (or? expr) (tagged-list? expr 'or))
(define (eval-or expr env)
  (define (eval-or-helper expr env)
    (let ((result (micro-eval (car expr) env))
          (rest-expr (cdr expr)))
      (if result
          result
          (if (null? rest-expr)
              result
              (eval-or-helper rest-expr env)))))
  (eval-or-helper (cdr expr) env))

; Primitive procedures
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'display display)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'append append)
        (list 'assoc assoc)
        (list 'null? null?)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))
(define (apply-in-underlying-scheme proc args)
  (apply proc args))

; Environment setup
(define (setup-environment)
  (let ((initial-env
        (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
        (define-variable! 'true #t initial-env)
        (define-variable! 'false #f initial-env)
        initial-env))
(define the-global-environment (setup-environment))

; Driver loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (micro-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input str)
  (newline)(newline)(display str)(newline))
(define (announce-output str)
  (newline)(display str)(newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))
