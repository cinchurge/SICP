(define (read-from-stream chr-stream)
  (let* ((token-stream-pair (read-token-from-stream chr-stream))
         (token (car token-stream-pair))
         (rest-stream (cdr token-stream-pair)))
    (if (token-leftpar? token)
        (read-list-from-stream '() rest-stream)
        token)))


(define (read-list-from-stream list-so-far chr-stream)
  (let* ((token-stream-pair (read-token-from-stream chr-stream))
         (token (car token-stream-pair))
         (rest-stream (cdr token-stream-pair)))
    (cond ((token-rightpar? token) (reverse list-so-far)) ; If we're in the middle of a list and we've hit a right paren, return the list
          ((token-leftpar? token) (read-list-from-stream (cons (read-list-from-stream '() rest-stream) list-so-far) rest-stream))
          (else
            (read-list-from-stream (cons token list-so-far) rest-stream)))))



(define (read-identifier-from-stream chr chr-stream)
  (define (read-identifier-from-stream-helper list-so-far chr-stream)
    (if (stream-null? chr-stream)
        (reverse list-so-far)
        (let ((next-char (stream-car chr-stream))
              (rest-chars (stream-cdr chr-stream)))
          (if (or (char-alphabetic? next-char)
                  (char-numeric? next-char))
              (read-identifier-from-stream-helper (cons next-char list-so-far) rest-chars)
              (reverse list-so-far)))))
  (string->symbol (list->string (read-identifier-from-stream-helper (list chr) chr-stream))))



(define (read-number-from-stream chr chr-stream)
  (define (read-number-from-stream-helper list-so-far chr-stream)
    (let ((next-char (stream-car chr-stream)
          (rest-chars (stream-cdr chr-stream))))
      (if (char-numeric? next-char)
          (read-number-from-stream-helper (cons next-char list-so-far) rest-chars)
          (reverse list-so-far))))
  (string->number (list->string (read-number-from-stream-helper (list chr) chr-stream))))



