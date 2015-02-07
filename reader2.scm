(use test)
(load "streams.scm")

(define (string->stream str)
  ;; Create a stream of characters from the string str
  (list->stream (string->list str)))



(define (list-so-far->symbol list-so-far)
  (string->symbol (list->string (reverse list-so-far))))
(define (list-so-far->number list-so-far)
  (string->number (list->string (reverse list-so-far))))



(define left-paren-token (list '*left-parenthesis-token*))
(define right-paren-token (list '*right-parenthesis-token*))



(define (make-token-stream chr-stream)
  (if (stream-null? chr-stream)
      the-empty-stream
      (let* ((token-stream-pair (read-token-from-stream chr-stream))
             (token (car token-stream-pair))
             (rest-stream (cdr token-stream-pair)))
        (cons-stream (cons token rest-stream) (make-token-stream rest-stream)))))



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



(define token-stream (make-token-stream (string->stream "((123 def ghi) jkl)")))

(define (token-leftpar? token)
  (eq? token left-paren-token))
(define (token-rightpar? token)
  (eq? token right-paren-token))


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

(define (read-string str)
  (read-from-stream (make-token-stream (string->stream str))))

(display (read-string "(abc def ghi)"))
