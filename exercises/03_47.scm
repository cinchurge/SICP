
(define (make-sem n)
    (let ((mutex (make-mutex))
          (n 0))
        (lambda (the-sem m)
            (cond ((eq? m 'acquire)
                   (begin
                       (mutex 'acquire)





(let ((sem (make-sem 3)))
    (begin
        (sem 'acquire)
        (sem 'acquire)
        (sem 'acquire)
        (sem 'acquire)))
