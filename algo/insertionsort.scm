(use test)

; Insert x into a sorted list:
; Given a sorted-list, iterate through each element and compare
; value to x. Keep going until we've hit a value that is greater
; than x and insert x before it.
(define (insert x sorted-list)
  (if (null? sorted-list)
      (list x)
      (let ((first-element (car sorted-list))
            (rest-elements (cdr sorted-list)))
        (if (>= x first-element)
            (cons first-element (insert x rest-elements))
            (cons x sorted-list)))))

; Insertion sort:
; Iterate through unsorted-list and recursively insert each element
; into a sorted list.
(define (insertion-sort unsorted-list)
  (define (insertion-sort-helper sorted-list unsorted-list)
    (if (null? unsorted-list)
        sorted-list
        (let ((next-element (car unsorted-list))
              (rest-elements (cdr unsorted-list)))
        (insertion-sort-helper (insert next-element sorted-list) rest-elements))))
  (insertion-sort-helper '() unsorted-list))


