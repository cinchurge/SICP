(use extras)

(define (make-mutex)
  (let ((cell (list #f)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (make-sem n)
    (let ((mutex (make-mutex)) (count 0))
        (define (the-sem m)
            (mutex 'acquire)
            (cond ((eq? m 'acquire) (set! count (+ count 1)))
                  ((eq? m 'release) (set! count (- count 1))))
            (mutex 'release)
            (printf "count=~S~N" count))
        the-sem))





(let ((sem (make-sem 3)))
    (sem 'acquire)
    (sem 'acquire)
    (sem 'acquire)
    (sem 'acquire))
