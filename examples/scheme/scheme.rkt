#lang racket
(require "../../parsing.rkt")
(provide (all-defined-out))

;; Grammar
;; s-expr             ::= s-symbol
;;                      | s-string
;;                      | s-integer
;;                      | s-floating
;;                      | s-rational
;;                      | s-complex
;;                      | s-bool
;;                      | s-char
;;                      | s-quote
;;                      | s-quasiquote
;;                      | s-unquote
;;                      | s-unquote-splicing
;;                      | s-list
;;                      | s-dotted-list
;;                      | s-vector

;; s-symbol           ::= e.g. xyz
;; s-string           ::= e.g. "xyz"
;; s-integer          ::= e.g. 123 | -123 | #d123 | #xAB | #o67 | #b10101
;; s-floating         ::= e.g. 1.23 | -1.23
;; s-rational         ::= e.g. 1/2 | -1/2
;; s-complex          ::= e.g. -2.5+0.0i | -2.5-0.0i
;; s-bool             ::= #t | #f
;; s-char             ::= e.g. #\newline | #\A
;; s-quote            ::= 's-expr
;; s-quasiquote       ::= `s-expr
;; s-unquote          ::= ,s-expr
;; s-unquote-splicing ::= ,@s-expr
;; s-list             ::= (s-expr*)
;; s-dotted-list      :: = (s-expr+ . s-expr)
;; s-vector           :: = #(s-expr*)

;; Delimited sequence
(define (try-s-symbol-char)
  (try-one-of (string->list "!$%&|*+-/:<=>?@^_~")))

(define (try-s-symbol)
  (let* ((first (try-choice try-letter try-s-symbol-char))
         (rest  (try-many (thunk (try-choice try-letter try-digit try-s-symbol-char))))
         (atom (list->string (cons first rest))))
    (string->symbol atom)))

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


(define (try-s-sign)
  (try-option #\+ (thunk (try-choice
                          (thunk (try-char #\+))
                          (thunk (try-char #\-))))))

(define (try-s-int)
  (let ((sign (try-s-sign))
        (xs (try-some try-digit)))
    (string->number (list->string (cons sign xs)))))

(define (try-s-int/base)
  (let* ((_ (try-char #\#))
         (x (try-one-of (string->list "dxob")))
         (xs (try-some (thunk (try-choice try-letter try-digit))))
         (num (string->number (list->string (cons #\# (cons x xs))))))
    (if num num (try-empty))))

(define (try-s-integer)
  (try-choice
   try-s-int
   try-s-int/base))

(define (try-s-floating)
  (let ((sign (try-s-sign))
        (x (try-some try-digit))
        (_ (try-char #\.))
        (y (try-some try-digit)))
    (string->number (list->string (append (list sign) x (list #\.) y)))))

(define (try-s-rational)
  (let ((sign (try-s-sign))
        (x (try-some try-digit))
        (_ (try-char #\/))
        (y (try-some try-digit)))
    (string->number (list->string (append (list sign) x (list #\/) y)))))

(define (try-s-complex)
  (let* ((sign1 (try-s-sign))
         (r (try-choice
             (thunk (let ((x (try-some try-digit))
                          (_ (try-char #\.))
                          (y (try-some try-digit)))
                      (append x (list #\.) y)))
             (thunk (try-some try-digit))))
         (sign2 (try-choice
                 (thunk (try-char #\+))
                 (thunk (try-char #\-))))
         (i (try-choice
             (thunk (let ((x (try-some try-digit))
                          (_ (try-char #\.))
                          (y (try-some try-digit)))
                      (append x (list #\.) y)))
             (thunk (try-some try-digit))))
         (_ (try-char #\i)))
    (string->number (list->string (append (list sign1) r (list sign2) i (list #\i))))))

(define (try-s-bool)
  (let* ((_ (try-char #\#)) )
    (try-choice
     (thunk (let ((_ (try-char #\t)))
              #t))
     (thunk (let ((_ (try-char #\f)))
              #f)))))

(define (try-s-char)
  (let ((_ (try-chars (list #\# #\\))))
    (try-choice
     (thunk (let ((xs (try-chars (string->list "newline"))))
              #\newline))
     (thunk (let ((xs (try-chars (string->list "space") )))
              #\space))
     (thunk (let ((xs (try-chars (string->list "return") )))
              #\return))
     (thunk (let ((xs (try-chars (string->list "tab") )))
              #\tab))
     (thunk (let ((x (try-item)) ; note that it even eat whitespace or ; , that's great!
                  (_ (try-not-followed-by try-alphanum)))
              x)))))

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

;; TODO: match bracket types
(define (try-token-s-left-bracket)
  (try-token (thunk (try-one-of (string->list "([")))))

(define (try-token-s-right-bracket)
  (try-token (thunk (try-one-of (string->list ")]")))))

(define (try-token-s-quote-mark)
  (try-token (thunk (try-char (car (string->list "'"))))))

(define (try-token-s-quasiquote-mark)
  (try-token (thunk (try-char (car (string->list "`"))))))

(define (try-token-s-unquote-mark)
  (try-token (thunk (try-char (car (string->list ","))))))

(define (try-token-s-unquote-splicing-mark)
  (try-token (thunk (try-chars (string->list ",@")))))

(define (try-token-s-dotted-mark)
  (try-token (thunk (try-char (car (string->list "."))))))

(define (try-token-s-vector-left-bracket)
  (try-token (thunk (try-chars (string->list "#(")))))

(define (try-token-s-symbol)
  (try-token try-s-symbol))

(define (try-token-s-string)
  (try-token try-s-string))

(define (try-token-s-integer)
  (try-token try-s-integer))

(define (try-token-s-floating)
  (try-token try-s-floating))

(define (try-token-s-rational)
  (try-token try-s-rational))

(define (try-token-s-complex)
  (try-token try-s-complex))

(define (try-token-s-bool)
  (try-token try-s-bool))

(define (try-token-s-char)
  (try-token try-s-char))

;; Expressions
(define (try-s-expr)
  (try-choice
   try-token-s-symbol
   try-token-s-string
   try-token-s-complex  ;; order 
   try-token-s-rational ;; sensitive
   try-token-s-floating ;;
   try-token-s-integer  ;;
   try-token-s-bool
   try-token-s-char
   try-s-quote
   try-s-quasiquote
   try-s-unquote-splicing ;; order 
   try-s-unquote          ;; sensitive
   try-s-list
   try-s-dotted-list
   try-s-vector
   ))

(define (try-s-quote)
  (let* ((_ (try-token-s-quote-mark))
         (xs (try-s-expr)))
    (list 'quote xs)))

(define (try-s-quasiquote)
  (let* ((_ (try-token-s-quasiquote-mark))
         (xs (try-s-expr)))
    (list 'quasiquote xs)))

(define (try-s-unquote)
  (let* ((_ (try-token-s-unquote-mark))
         (xs (try-s-expr)))
    (list 'quasiquote xs)))

(define (try-s-unquote-splicing)
  (let* ((_ (try-token-s-unquote-splicing-mark))
         (xs (try-s-expr)))
    (list 'unquote-splicing xs)))

(define (try-s-list)
  (let* ((_ (try-token-s-left-bracket))
         (xs (try-many try-s-expr))
         (_ (try-token-s-right-bracket)))
    xs))

(define (try-s-dotted-list)
  (let* ((_ (try-token-s-left-bracket))
         (head (try-some try-s-expr))
         (_ (try-token-s-dotted-mark))
         (tail (try-some try-s-expr))
         (_ (try-token-s-right-bracket)))
    (if (equal? (length head) 1)
        (cons (car head) tail)
        (cons head tail))))

(define (try-s-vector)
  (let* ((_ (try-token-s-vector-left-bracket))
         (xs (try-many try-s-expr))
         (_ (try-token-s-right-bracket)))
    (list->vector xs)))

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

     (check-equal? (parse-test try-s-bool (string->list "#t")) '(#t #t ()))
     (check-equal? (parse-test try-s-bool (string->list "#f")) '(#t #f ()))
     (check-equal? (parse-test try-s-symbol (string->list "asdsad")) '(#t asdsad ()))

     (check-equal? (parse-test try-s-string (string->list "\" abc asd asd ''' dg \"")) '(#t " abc asd asd ''' dg " ()))

     (check-equal? (parse-test try-s-integer (string->list "100")) '(#t 100 ()))
     ;; (check-equal? (parse-test try-s-integer (string->list "-99")) '(#t -99 ()))
     
     (check-equal? (parse-test try-token-s-integer '(#\1 #\space #\2 #\space #\3 #\))) '(#t 1 (#\2 #\space #\3 #\))))
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

     
     (check-equal?(parse-test try-s-integer (string->list "123")) '(#t 123 ()))
     (check-equal?(parse-test try-s-integer (string->list "-123")) '(#t -123 ()))
     
     (check-equal?(parse-test try-s-integer (string->list "#d123")) '(#t 123 ()))
     (check-equal?(parse-test try-s-integer (string->list "#xAB")) '(#t 171 ()))
     (check-equal?(parse-test try-s-integer (string->list "#o127")) '(#t 87 ()))
     (check-equal? (parse-test try-s-integer (string->list "#b101010")) '(#t 42 ()))
     (check-equal? (parse-test try-s-integer (string->list "#xAB")) '(#t 171 ()))

     (check-equal? (parse-test try-s-floating (string->list "1.23")) '(#t 1.23 ()))
     (check-equal? (parse-test try-s-floating (string->list "+1.23")) '(#t 1.23 ()))
     (check-equal? (parse-test try-s-floating (string->list "-1.23")) '(#t -1.23 ()))
     (check-equal? (parse-test try-s-rational (string->list "1/2")) '(#t 1/2 ()))
     (check-equal? (parse-test try-s-rational (string->list "+1/2")) '(#t 1/2 ()))
     (check-equal? (parse-test try-s-rational (string->list "-1/2")) '(#t -1/2 ()))
     (check-equal? (parse-test try-s-complex (string->list "2.5+0.0i")) '(#t 2.5+0.0i ()))
     (check-equal? (parse-test try-s-complex (string->list "+2.5+0.0i")) '(#t 2.5+0.0i ()))
     (check-equal? (parse-test try-s-complex (string->list "-2.5-0.0i")) '(#t -2.5-0.0i ()))

     (check-equal? (parse-test try-s-char (string->list "#\\A")) '(#t #\A ()))
     (check-equal? (parse-test try-s-char (string->list "#\\B #\\C")) '(#t #\B (#\space #\# #\\ #\C)))
     (check-equal? (parse-test try-s-char (string->list "#\\;")) '(#t #\; ()))
     (check-equal? (parse-test try-s-char (string->list "#\\ ")) '(#t #\space ()))
     (check-equal? (parse-test try-s-expr (string->list "(#\\ #\\;)")) '(#t (#\space #\;) ()))

     ;; FIXME: This corner case is very werid
     ;; in my parser, it is failed, but in racket, it return '(#\# |;|), I dont know what is mean..
     ;; (parse-test try-s-expr (string->list "(#\\#\\;)"))

     (check-equal? (parse-test try-s-floating (string->list "1.23")) '(#t 1.23 ()))

     (check-equal? (parse-test try-s-expr (string->list "`(1 2 3)")) '(#t `(1 2 3) ()))
     (check-equal? (parse-test try-s-expr (string->list "`(,1 2 3)")) '(#t `(`1 2 3) ()))
     (check-equal? (parse-test try-s-expr (string->list "`(,@1 2 3)")) '(#t `(,@1 2 3) ()))
     (check-equal? (parse-test try-s-expr (string->list "
                                    (let ((a 1)
                                          (b 2)
                                          (@c '(3 4 5)))
                                      `(,a                
                                        ,b               
                                        ,@c))
") #t)
                   '(#t (let ((a 1) (b 2) (@c '(3 4 5))) `(`a `b ,@c)) ()))


     (check-equal? (parse-test try-s-vector (string->list "#(1 2 3)")) '(#t #(1 2 3) ()))

     
     ))
  
  (run-tests parsing-tests)
  )


