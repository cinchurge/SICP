(use test)

(load "rational.scm")

(let ((one-half (make-rat 1 2)))
  (test 1 (numer one-half))
  (test 2 (denom one-half)))

(let ((x (make-rat 2 4)))
  (test 1 (numer x))
  (test 2 (denom x)))

(let ((x (make-rat -2 4)))
  (test -1 (numer x))
  (test 2 (denom x)))

(let ((x (make-rat 2 -4)))
  (test -1 (numer x))
  (test 2 (denom x)))

(let ((x (make-rat -2 -4)))
  (test 1 (numer x))
  (test 2 (denom x)))
