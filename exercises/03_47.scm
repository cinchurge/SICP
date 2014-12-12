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
        (define (sem-acquire)
            (mutex 'acquire)
            (if (>= count n)
                (begin (mutex 'release)
                       (sem-acquire))
                (begin (set! count (+ count 1))
                       (mutex 'release))))

        (define (sem-release)
            (mutex 'acquire)
            (set! count (- count 1))
            (mutex 'release))

        (define (the-sem m)
            (cond ((eq? m 'acquire) (sem-acquire))
                  ((eq? m 'release) (sem-release)))
            (printf "count=~S~N" count))
        the-sem))





(let ((sem (make-sem 3)))
    (sem 'acquire)
    (sem 'acquire)
    (sem 'acquire))
