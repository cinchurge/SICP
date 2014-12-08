;(load "parallel.scm")
(use extras)
(use test)
(use sicp-concurrency)

(define x 10)

(define testp
    (parallel-execute
        (lambda () (set! x (* x x)))
        (lambda () (set! x (+ x 1)))))

(define (test-parallel n)
    (define (iter n)
        (if (< n 0)
            (begin
                (display "done")
                (newline))
            (begin
                (testp)
                (display x)
                (newline)
                (iter (- n 1)))))
    (iter n))

(define s (make-serializer))
(parallel-execute
    (s (lambda () (set! x (* x x))))
    (s (lambda () (set! x (+ x 1)))))

; make-account
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((protected (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) (protected withdraw))
                  ((eq? m 'deposit) (protected deposit))
                  ((eq? m 'balance) balance)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m))))
        dispatch))

(printf "Testing make-account~N")
(define peter (make-account 0))
(test "Peter's balance=0" 0 (peter 'balance))
((peter 'deposit) 100)
(test "Peter's balance=100" 100 (peter 'balance))
((peter 'withdraw) 50)
(test "Peter's balance=50" 50 (peter 'balance))
(printf "~N")

; Complexity of using multiple shared resources
(define (exchange account1 account2)
    (let ((difference (- (account1 'balance)
                         (account2 'balance))))
        ((account1 'withdraw) difference)
        ((account2 'deposit) difference)))

(printf "Testing exchange~N")
(define paul (make-account 200))
(test "Peter's balance before exchange=50" 50 (peter 'balance))
(test "Paul's balance before exchange=200" 200 (paul 'balance))
(exchange paul peter)
(test "Peter's balance after exchange=200" 200 (peter 'balance))
(test "Paul's balance after exchange=50" 50 (paul 'balance))
(printf "~N")

; make-account-and-serializer
(define (make-account-and-serializer balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m))))
        dispatch))

(printf "Testing make-account-and-serializer~N")
(define peter (make-account-and-serializer 0))
(test "Peter's balance=0" 0 (peter 'balance))
((peter 'deposit) 100)
(test "Peter's balance=100" 100 (peter 'balance))
((peter 'withdraw) 50)
(test "Peter's balance=50" 50 (peter 'balance))

(define (deposit account amount)
    (let ((s (account 'serializer))
          (d (account 'deposit)))
        ((s d) amount)))

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
        ((serializer1 (serializer2 exchange))
         account1
         account2)))
