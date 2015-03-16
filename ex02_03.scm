(load "ex02_02.scm")

; Implement a representation of rectangles in a plane,
; create procedures that compute the perimiter and the area
; of a given rectangle.
; (make-rectangle blc trc)
; (perimiter-rectangle rect)
; (area-rectangle rect)

; (perimiter-rectangle rect): return perimeter of
; the rectangle rect
(define (perimeter-rectangle rect)
  (let* ((w (width-rectangle rect))
         (h (height-rectangle rect)))
    (+ (* w 2) (* h 2))))

(define (area-rectangle rect)
  (let* ((w (width-rectangle rect))
         (h (height-rectangle rect)))
    (* w h)))


; Design 1: use a pair containing bottom left corner and top right corner
; to represent the rectangle
(define (make-rectangle blc trc)
  (cons blc trc))

(define (blc-rectangle rect)
  (car rect))

(define (trc-rectangle rect)
  (cdr rect))

(define (width-rectangle rect)
  (let* ((blc (blc-rectangle rect))
         (trc (trc-rectangle rect)))
    (- (x-point trc) (x-point blc))))

(define (height-rectangle rect)
  (let* ((blc (blc-rectangle rect))
         (trc (trc-rectangle rect)))
    (- (y-point trc) (y-point blc))))

; Unit test for design 1:
(let* ((blc (make-point 1 1))
       (trc (make-point 3 5))
       (rect (make-rectangle blc trc)))
  (test 12 (perimeter-rectangle rect))
  (test 8 (area-rectangle rect)))

; Design 2: rectangle defined with a nested pair of 3 elements:
; an offset, width, and height.
(define (make-rectangle offset width height)
  (cons offset (cons width height)))

(define (width-rectangle rect)
  (cadr rect))

(define (height-rectangle rect)
  (cddr rect))

; Unit test for design 2:
(let* ((w 2)
       (h 4)
       (o (make-point 1 1))
       (rect (make-rectangle o w h)))
  (test 12 (perimeter-rectangle rect))
  (test 8 (area-rectangle rect)))




