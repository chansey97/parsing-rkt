#lang racket
(require "../parsing.rkt")
(provide (all-defined-out))

;; Grammar
;; s-expr  ::= s-atom
;;           | s-string
;;           | s-number
;;           | s-list
;;           | s-quote
;;           | s-dotted-list
;; s-list  ::= (s-expr*)
;; s-quote ::= 's-expr
;; s-dotted-list :: = (s-expr+ . s-expr)

;; Delimited sequence
(define (try-s-symbol-char)
  (try-one-of (string->list "!#$%&|*+-/:<=>?@^_~")))

(define (try-s-atom)
  (let* ((first (try-choice try-letter try-s-symbol-char))
         (rest  (try-many (thunk (try-choice try-letter try-digit try-s-symbol-char))))
         (atom (list->string (cons first rest))))
    (cond ((equal? atom "#t") #t)
          ((equal? atom "#f") #f)
          (else (string->symbol atom)))))

(define (try-s-escaped)
  (let* ((_ (try-char #\\)) 
         (x (try-one-of (list #\\ #\" #\n #\r #\t))))
    (match x
      [#\\ x]
      [#\" x]
      [#\n #\newline]
      [#\r #\return]
      [#\t #\tab])))

(define (try-s-string)
  (let* ((_ (try-char #\"))
         (x  (try-many (thunk
                        (try-choice
                         try-s-escaped
                         (thunk (try-none-of (string->list "\"\\")))))))
         (_ (try-char #\")))
    (list->string x)))

(define (try-s-number)
  (try-int))

;; Spacing and comments
(define (try-comment)
  (try-chars (string->list ";"))
  (try-many (thunk (try-sat (λ (x) (not (equal? #\newline x))))))
  'comment)

(define (try-junk)
  (try-many (thunk (try-choice try-space+ try-comment)))
  'junk)

;; Tokens
(define (try-token try-p)
  (let* ((v (try-p))
         (_ (try-junk)))
    v))

(define (try-token-s-left-bracket)
  (try-token (thunk (try-char (car (string->list "("))))))

(define (try-token-s-right-bracket)
  (try-token (thunk (try-char (car (string->list ")"))))))

(define (try-token-s-quote-mark)
  (try-token (thunk (try-char (car (string->list "'"))))))

(define (try-token-s-dotted-mark)
  (try-token (thunk (try-char (car (string->list "."))))))

(define (try-token-s-atom)
  (try-token try-s-atom))

(define (try-token-s-string)
  (try-token try-s-string))

(define (try-token-s-number)
  (try-token try-s-number))

;; Expressions
(define (try-s-expr)
  (try-choice
   try-token-s-atom
   try-token-s-string
   try-token-s-number
   try-s-quote
   try-s-list))

(define (try-s-list)
  (let* ((_ (try-token-s-left-bracket))
         (xs (try-many try-s-expr))
         (_ (try-token-s-right-bracket)))
    xs))

(define (try-s-quote)
  (let* ((_ (try-token-s-quote-mark))
         (xs (try-s-expr)))
    (list 'quote xs)))

(define (try-s-dotted-list)
  (let* ((_ (try-token-s-left-bracket))
         (head (try-some try-s-expr))
         (_ (try-token-s-dotted-mark))
         (tail (try-some try-s-expr))
         (_ (try-token-s-right-bracket)))
    (if (equal? (length head) 1)
        (cons (car head) tail)
        (cons head tail))))


;; Tests
(module+ test
  (require rackunit rackunit/text-ui)
  (require syntax/parse/define)

  (define (parse-test try-proc inp [junk? #f])
    (with-handlers ([exn:fail:parsing?
                     (lambda (exn)
                       (list #f unconsumed-inp))])
      (if junk?
          (list #t (parse (thunk (try-junk) (try-proc)) inp) unconsumed-inp) 
          (list #t (parse try-proc inp) unconsumed-inp))))

  (define parsing-tests
    (test-suite
     "Tests for parsing"

     (check-equal? (parse-test try-s-atom (string->list "#t")) '(#t #t ()))
     (check-equal? (parse-test try-s-atom (string->list "#f")) '(#t #f ()))
     (check-equal? (parse-test try-s-atom (string->list "#asd")) '(#t |#asd| ()))
     (check-equal? (parse-test try-s-atom (string->list "asdsad")) '(#t asdsad ()))

     (check-equal? (parse-test try-s-string (string->list "\" abc asd asd ''' dg \"")) '(#t " abc asd asd ''' dg " ()))

     (check-equal? (parse-test try-s-number (string->list "100")) '(#t 100 ()))
     (check-equal? (parse-test try-s-number (string->list "-99")) '(#t -99 ()))
     
     (check-equal? (parse-test try-token-s-number '(#\1 #\space #\2 #\space #\3 #\))) '(#t 1 (#\2 #\space #\3 #\))))
     (check-equal? (parse-test try-s-list (string->list "(1 2 3)")) '(#t (1 2 3) ()))
     (check-equal? (parse-test try-s-list (string->list "(1 (2) 3 (4 2 3))")) '(#t (1 (2) 3 (4 2 3)) ()))
     (check-equal? (parse-test try-s-list (string->list "(1 (2) 3 '(4 '2 3))")) '(#t (1 (2) 3 '(4 '2 3)) ()))

     (check-equal? (parse-test try-s-quote (string->list "'a")) '(#t 'a ()))
     (check-equal? (parse-test try-s-list (string->list
                                           "(define (try-s-quote)
    (let* ((_ (try-char (car (string->list \"'\"))))
           (_ (try-space))
           (xs (try-s-expr)))
      (list 'quote xs)))"))
                   '(#t
                     (define (try-s-quote)
                       (let* ((_ (try-char (car (string->list "'"))))
                              (_ (try-space))
                              (xs (try-s-expr)))
                         (list 'quote xs)))
                     ()))

     (check-equal? (parse-test try-s-list (string->list
                                           "(define (try-s-quote) ;; xxx yyy
    (let* ((_ (try-char (car (string->list \"'\")))) ;; xxxx yyy y
           (_ (try-space));; xxx yyy 
           (xs (try-s-expr)));; xxx yyy
      (list 'quote xs)))"))
                   '(#t
                     (define (try-s-quote)
                       (let* ((_ (try-char (car (string->list "'"))))
                              (_ (try-space))
                              (xs (try-s-expr)))
                         (list 'quote xs)))
                     ()))

     (check-equal? (parse-test try-s-list (string->list
                                           "( ;; xxx yyy
  define ;; xxx yyy (try-s-quote) ;; xxx yyy
    (let* ((_ (try-char (car (string->list \"'\")))) ;; xxxx yyy y
           (_ ( ;; xxx yyy
  try-space));; xxx yyy 
           (xs (try-s-expr)));; xxx yyy
      (list 'quote xs)))"))
                   '(#t
                     (define (let*
                                 ((_ (try-char (car (string->list "'"))))
                                  (_ (try-space))
                                  (xs (try-s-expr)))
                               (list 'quote xs)))
                     ()))

     (check-equal? (parse-test try-s-expr (string->list " 2")) '(#f (#\2)))
     (check-equal? (parse-test try-s-expr (string->list " 2") #t) '(#t 2 ()))

     (check-equal? (parse-test try-s-dotted-list (string->list "(1 . 3)")) '(#t (1 3) ()))
     (check-equal? (parse-test try-s-dotted-list (string->list "(1 . (3))")) '(#t (1 (3)) ()))
     (check-equal? (parse-test try-s-dotted-list (string->list "(1 2 . 3)")) '(#t ((1 2) 3) ()))


     (check-equal? (parse-test try-s-string (string->list "\" \\ \"")) '(#f (#\space #\"))) ; this should be a parse error
     (check-equal? (parse-test try-s-string (string->list "\" \\\\ \"")) '(#t " \\ " ())) ; this is right

     ))
  
  (run-tests parsing-tests)



  )


