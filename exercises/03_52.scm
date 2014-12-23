(use extras)
(use test)

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
           result))))

; Non-optimized delay
(define-syntax delay 
  (syntax-rules ()
    ((delay expr)
     (lambda ()
        expr))))

; force must be implemented since the default force
; expects a promise object and not a straight lambda
(define (force e)
  (e))

(load "../examples/streams.scm")

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

; If seq were not delayed, the terms would've been 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210

(define seq
    (stream-map accum
                (stream-enumerate-interval 1 20)))
;We must evaluate to the first term of seq, so sum = 1
;Optimized:
(test "sum==1" 1 sum)
;Non-optimized: sum==1

(define y (stream-filter even? seq)) 
;We must evalulate to the first term of y, so sum = 6
;Optimized:
(test "sum==6" 6 sum)
;Non-optimized:
;1, 3, 6
;sum==6

(define z
    (stream-filter (lambda (x) (= (remainder x 5) 0))
                   seq))
;We must evaluate to the first term of z, so sum = 10
;Optimized:
(test "sum==10" 10 sum)
;Non-optimized:
;1, 8, 11, 15
;sum==15

(test "(stream-ref y 7)==136" 136 (stream-ref y 7))
;We must evaluate all the way to the 8th term of y, so
; Opotimized: sum = 136
(test "sum==136" 136 sum)

(display-stream z)
;For optimized delay, we only evaluate seq once, so z consists
;of terms divisable by 5, i.e. 10, 15, 45, 55, 105, 120, 190, 210
;Optimized: sum is now 210
(test "sum==210" 210 sum)
