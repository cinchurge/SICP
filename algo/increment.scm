(define (increment y)
  (if (= y 0)
      1
      (if (= (modulo y 2) 1)
          (* 2 (increment (quotient y 2)))
          (+ y 1))))

(display (increment 0))(newline)
(display (increment 1))(newline)
(display (increment 2))(newline)
(display (increment 3))(newline)
