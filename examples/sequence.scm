(use extras)

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;(printf "~S~N" (enumerate-interval 2 7))
