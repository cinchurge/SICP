(use test)
(load "streams.scm")

; Create a stream of characters from the string str
(define (string->stream str)
  (list->stream (string->list str)))

; Convert an inverted list of chars to a symbol
(define (list-so-far->symbol list-so-far)
  (string->symbol (list->string (reverse list-so-far))))
; Convert an inverted list of chars to a number
(define (list-so-far->number list-so-far)
  (string->number (list->string (reverse list-so-far))))

; The paren tokens
(define left-paren-token (list '*left-parenthesis-token*))
(define right-paren-token (list '*right-parenthesis-token*))

; Create a stream of tokens from a stream of chars
(define (make-token-stream chr-stream)
  (if (stream-null? chr-stream)
      the-empty-stream
      (let* ((token-stream-pair (read-token-from-stream chr-stream))
             (token (car token-stream-pair))
             (rest-stream (cdr token-stream-pair)))
        (cons-stream (cons token rest-stream) (make-token-stream rest-stream)))))

; Read a token from a stream of chars. In addition to returning a token, this also
; returns the updated char-stream. This is necessary since any number of chars may be read,
; and we don't want use assignment to update the char stream.
(define (read-token-from-stream chr-stream)
  (let ((first-char (stream-car chr-stream))
        (rest-stream (stream-cdr chr-stream)))
    (cond ((char-whitespace? first-char) (read-token-from-stream rest-stream))
          ((eq? first-char #\( ) (cons left-paren-token rest-stream))
          ((eq? first-char #\) ) (cons right-paren-token rest-stream))
          ((char-alphabetic? first-char) (read-identifier-from-stream first-char rest-stream))
          ((char-numeric? first-char) (read-number-from-stream first-char rest-stream))
          (else
            (error "illegal lexical syntax")))))

; Read an identifier from a stream of chars. Return the rest of the char stream
; along with the identifier.
(define (read-identifier-from-stream chr chr-stream)
  (define (read-identifier-from-stream-helper list-so-far chr-stream)
    (if (stream-null? chr-stream)
        (cons (list-so-far->symbol list-so-far) chr-stream)
        (let ((next-char (stream-car chr-stream))
              (rest-stream (stream-cdr chr-stream)))
          (if (or (char-alphabetic? next-char)
                  (char-numeric? next-char))
              (read-identifier-from-stream-helper (cons next-char list-so-far) rest-stream)
              (cons (list-so-far->symbol list-so-far) chr-stream)))))
  (read-identifier-from-stream-helper (list chr) chr-stream))

; Read a number from a stream of chars. Return the rest of the char stream
; along with the number.
(define (read-number-from-stream chr chr-stream)
  (define (read-number-from-stream-helper list-so-far chr-stream)
    (if (stream-null? chr-stream)
        (cons (list-so-far->number list-so-far) chr-stream)
        (let ((next-char (stream-car chr-stream))
              (rest-stream (stream-cdr chr-stream)))
          (if (char-numeric? next-char)
              (read-number-from-stream-helper (cons next-char list-so-far) rest-stream)
              (cons (list-so-far->number list-so-far) chr-stream)))))
  (read-number-from-stream-helper (list chr) chr-stream))

; Check if a token is a left or right paren
(define (token-leftpar? token) (eq? token left-paren-token))
(define (token-rightpar? token) (eq? token right-paren-token))

; Read from a token stream and convert the result into symbols. This procedure
; operates together with read-list-from-stream to create nested lists.
(define (read-from-stream token-stream)
  (let* ((token-stream-pair (stream-car token-stream))
         (token (car token-stream-pair))
         (rest-token-stream (stream-cdr token-stream)))
    (cond ((token-leftpar? token)
             (let* ((sublist-stream-pair (read-list-from-stream '() rest-token-stream))
                    (sublist (car sublist-stream-pair)))
                    sublist))
          (else
            token))))

; Read a list from a token stream. This is called by read-from-strem when a left-paren
; is encountered.
(define (read-list-from-stream list-so-far token-stream)
  (let* ((token-stream-pair (stream-car token-stream))
         (token (car token-stream-pair))
         (rest-token-stream (stream-cdr token-stream)))
    (cond ((token-rightpar? token) (cons (reverse list-so-far) rest-token-stream))
          ((token-leftpar? token)
             (let* ((sublist-stream-pair (read-list-from-stream '() rest-token-stream))
                    (sublist (car sublist-stream-pair))
                    (sublist-rest-token-stream (cdr sublist-stream-pair)))
                    (read-list-from-stream (cons sublist list-so-far) sublist-rest-token-stream)))
          (else
            (read-list-from-stream (cons token list-so-far) rest-token-stream)))))

; Convert a nested s-expression into nested lists
(define (read-string str)
  (read-from-stream (make-token-stream (string->stream str))))

; Unit tests
(let ((expr (read-string "((123 def ghi) jkl)")))
  (test "(car expr)" '(123 def ghi) (car expr))
  (test "(cdr expr)" '(jkl) (cdr expr))
  (test "(cdar expr)" '(def ghi) (cdar expr))
  (test "(caar expr)" '123 (caar expr)))
