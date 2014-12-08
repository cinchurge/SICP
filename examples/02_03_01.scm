(define (memq item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

(display (memq 'apple '(pear banana prune)))
(newline)

(display (memq 'apple '(x (apple sauce) y apple pear)))
(newline)
