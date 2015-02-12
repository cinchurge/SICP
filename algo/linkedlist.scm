(define (linked-list-prepend x linked-list)
  ; Prepend x to the beginning of linked-list
  (cons x linked-list))

(define (linked-list-append x linked-list)
  ; Append x to linked-list
  (if (null? linked-list)
      (list x)
      (cons (car linked-list) (linked-list-append x (cdr linked-list)))))

(define (linked-list-search x linked-list)
  ; Search for element x in linked-list and return
  ; the sublist beginning with element x
  (if (null? linked-list)
      linked-list
      (let ((next-element (car linked-list))
            (rest-elements (cdr linked-list)))
        (if (= next-element x)
            linked-list
            (linked-list-search x rest-elements)))))

(define (linked-list-delete x linked-list)
  ; Delete x from linked-list
  (define (linked-list-delete-helper x head tail)
    (if (null? tail)
        (append head tail)
        (if (= x (car tail))
            (append head (cdr tail))
            (linked-list-delete-helper x (linked-list-append (car tail) head) (cdr tail)))))
  (linked-list-delete-helper x '() linked-list))
