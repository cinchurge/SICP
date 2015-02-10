(use test)
(use extras)

(load "math.scm")
(load "sequence.scm")
(load "streams.scm")

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

; Sum of primes between 2..10: 2+3+5+7=17
(test "(sum-primes 2 10)==17" 17 (sum-primes 2 10))


; Tests
(test "(stream-car (cons-stream 1 2))==1" 1 (stream-car (cons-stream 1 2)))
(test "(stream-cdr (cons-stream 1 2))==2" 2 (stream-cdr (cons-stream 1 2)))
(test "(stream-car (stream-enumerate-interval 10000 1000000))==10000" 10000 (stream-car (stream-enumerate-interval 10000 1000000)))
(test "(stream-car (stream-cdr (stream-enumerate-interval 10000 1000000)))==10001" 10001 (stream-car (stream-cdr (stream-enumerate-interval 10000 1000000))))

(printf "~S~N"
        (stream-car
          (stream-cdr
            (stream-filter prime?
                           (stream-enumerate-interval
                            10000 1000000)))))
