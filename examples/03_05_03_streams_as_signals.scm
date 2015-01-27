(use test)
(use extras)
(load "../examples/streams.scm")

; Python extensions for plotting
(require-extension pyffi)
(py-start)
(py-import "pylab")
(define-pyfun "pylab.plot" data)
(define-pyfun "pylab.show")

; gen-sine-wave: generate a stream representing a sine wave with
; frequency f, time interval dt
(define (gen-sine-wave f dt n)
  (cons-stream (sin (* 2.0 3.14159 f (* n dt)))
               (gen-sine-wave f dt (+ n 1))))

; Generate a 100Hz sine wave with 100 points per period. This
; should provide adequate sampling to prevent severe aliasing
(define sine-wave-100Hz (gen-sine-wave 100.0 0.0001 0))

; The integral stream generator
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; The integral of a sine wave over its
; period should be 0
(define sine-integral (integral (stream-cdr sine-wave-100Hz) (stream-car sine-wave-100Hz) 0.0001))

;(pylab.plot (stream->list-upto sine-wave-100Hz 200))
(pylab.plot (stream->list-upto sine-integral 200))
(pylab.show)

;(display-stream-upto sine-wave-100Hz 200)
;(display-stream-upto sine-integral 200)
;(newline)

; Exercise 3.73
(define (RC r c dt)
  (define (rc i-stream v0)
    (add-streams (scale-stream i-stream r)
                 (integral (scale-stream i-stream (/ 1.0 c)) v0 dt)))
  rc)

(define RC1 (RC 5 1 0.5))

; Starting from 0 volts, a DC current of 1A should charge up the capacitor
; to V = R * I volts
(define sine-wave-1000Hz (gen-sine-wave 1000.0 0.00001 0))
(define dc-1amp (cons-stream 1 dc-1amp))
;(pylab.plot (stream->list-upto (RC1 sine-wave-1000Hz 0) 200))
(pylab.plot (stream->list-upto (RC1 dc-1amp 5) 1000))
(pylab.show)


; Exercise 3.74
; Alyssa's code:
(define (sign-change-detector current-value last-value)
  ; if current-value * last-value < 0, we have opposite signs
  ; otherwise the values are the same signs and we don't really care
  (if (< (* current-value last-value) 0)
    (if (> last-value current-value) -1 1)
    0))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define sense-data sine-wave-100Hz)

(define zero-crossings
  (make-zero-crossings (stream-cdr sense-data) (stream-car sense-data)))

; Eva's code:
(define zero-crossings
  (stream-map sign-change-detector
              (stream-cdr sense-data)
              sense-data))

; 2 periods should give us two negative zero-crossings and one positive zero-crossing
;(printf "sense-data:~N")
;(display-stream-upto sense-data 200)
;(newline)

;(printf "zero-crossings of sense-data:~N")
;(display-stream-upto zero-crossings 200)
;(newline)

; Exercise 3.75

; gen-noisy-sine-wave: generate a stream representing a sine wave with
; frequency f, time interval dt, with noise of amplitude noise-amp
(define (gen-noisy-sine-wave f dt noise-amp n)
  (cons-stream (+ (sin (* 2.0 3.14159 f (* n dt))) (* noise-amp (/ (- (+ (random 100) 1) 50) 50)))
               (gen-noisy-sine-wave f dt noise-amp (+ n 1))))

(define noisy-sense-data (gen-noisy-sine-wave 100.0 0.0001 0.2 0))

(define zero-crossings
  (stream-map sign-change-detector
              (stream-cdr noisy-sense-data)
              noisy-sense-data))

;(printf "noisy-sense-data:~N")
;(display-stream-upto noisy-sense-data 200)
;(newline)

;(printf "noisy-sense-data zero-crossings:~N")
;(display-stream-upto zero-crossings 200)
;(newline)

; Louis's modified code:
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
              2)))
    (cons-stream
      (sign-change-detector avpt last-value)
      (make-zero-crossings
        (stream-cdr input-stream) avpt))))

(define zero-crossings
  (make-zero-crossings (stream-cdr noisy-sense-data)
                       (stream-car noisy-sense-data)))

;(printf "Louis's zero-crossings:~N")
;(display-stream-upto zero-crossings 200)
;(newline)

; Louis made a mistake in making avpt an cumulative average
; of all data in the stream. For the smoothing to work, we
; must to a 'windowed' average instead.
(define (make-zero-crossings-2 input-stream last-value last-last-value)
  (let ((avpt1 (/ (+ (stream-car input-stream) last-value) 2))
        (avpt2 (/ (+ last-value last-last-value) 2)))
    (cons-stream
      (sign-change-detector avpt2 avpt1)
      (make-zero-crossings-2
        (stream-cdr input-stream) (stream-car input-stream) last-value))))

(define zero-crossings
  (make-zero-crossings-2 (stream-cdr (stream-cdr noisy-sense-data))
                       (stream-car (stream-cdr noisy-sense-data))
                       (stream-car noisy-sense-data)))

;(printf "windowed-smoothing:~N")
;(display-stream-upto zero-crossings 200)
;(newline)

; Exercise 3.76
(define (smooth input-stream)
  (let ((pt1 (stream-car input-stream))
        (pt2 (stream-car (stream-cdr input-stream))))
    (cons-stream (/ (+ pt1 pt2) 2)
                 (smooth (stream-cdr input-stream)))))

(define smoothed-noisy-sense-data (smooth noisy-sense-data))

(printf "smoothed noisy data:~N")
(display-stream-upto smoothed-noisy-sense-data 200)
(newline)

(define zero-crossings (make-zero-crossings (stream-cdr smoothed-noisy-sense-data) (stream-car smoothed-noisy-sense-data)))

(printf "modularized zero-crossings:~N")
(display-stream-upto zero-crossings 200)
(newline)

(pylab.plot (stream->list-upto sine-wave-100Hz 200))
(pylab.plot (stream->list-upto noisy-sense-data 200))
(pylab.plot (stream->list-upto zero-crossings 200))
(pylab.show)

; Remember to clean up shop
(py-stop)

