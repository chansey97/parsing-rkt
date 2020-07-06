#lang racket
(provide (all-defined-out))

;; Basic definitions
(struct exn:fail:parsing exn:fail ())

(define unconsumed-inp '())

(define (get-unconsumed-inp)
  unconsumed-inp)

(define (set!-unconsumed-inp inp)
  (set! unconsumed-inp inp))

(define (parse try-proc inp)
  (set! unconsumed-inp inp)
  (try-proc))

(define (try-item)
  (match unconsumed-inp
    [(list) (raise (exn:fail:parsing "try-item failed" (current-continuation-marks)))]
    [(list x xs ...) (set! unconsumed-inp xs) x]))

;; Making choices by exceptions
(define (try-empty)
  (raise (exn:fail:parsing "try-empty" (current-continuation-marks))))

(define (try-choice try-p try-q . rest-of-tries)
  ;; (printf "try-choice enter unconsumed-inp = ~v\n" unconsumed-inp)
  (let ((saved-inp unconsumed-inp))
    (with-handlers ([exn:fail:parsing?
                     (lambda (exn)
                       ;; (printf "try-choice try-p failed where try-p = ~v\n" try-p)
                       ;; (printf "try-choice try-p failed saved-inp = ~v\n" saved-inp)
                       (set! unconsumed-inp saved-inp)
                       (match rest-of-tries
                         [(list) (try-q)]
                         ;; [(list x xs ...) (try-choice try-q x xs)]
                         [(list x xs ...) (apply try-choice (cons try-q (cons x xs)))]
                         ))])
      (try-p))))

;; Derived primitives
(define (try-sat p?)
  (let ((x (try-item)))
    (if (p? x) x (try-empty))))

(define (try-many try-x)
  (try-choice
   (thunk (try-some try-x))
   (const '())))

(define (try-some try-x)
  (cons (try-x) (try-many try-x)))

(define (try-one-of cs)
  (try-sat (λ (c) (member c cs))))

(define (try-none-of cs)
  (try-sat (λ (c) (not (member c cs)))))

(define (try-not-followed-by try-x)
  (try-choice
   (thunk
    (let ((saved-inp unconsumed-inp)
          (_ (try-x)))
      (set! unconsumed-inp saved-inp)))
   (thunk 'not-followed)))

(define (try-option x try-x)
  (try-choice
   try-x
   (const x)))

;; Utils
(define (try-digit)
  (try-sat char-numeric?))

(define (try-lower)
  (try-sat char-lower-case?))

(define (try-upper)
  (try-sat char-upper-case?))

(define (try-letter)
  (try-sat char-alphabetic?))

(define (try-alphanum)
  (try-sat (disjoin char-alphabetic? char-numeric?)))

(define (try-char x)
  (try-sat ((curry char=?) x)))

(define (try-chars chars)
  (match chars
    [(list) '()]
    [(list x xs ...) (try-char x)
                     (try-chars xs)
                     chars]))

(define (try-ident)
  (let ((x (try-lower))
        (xs (try-many try-alphanum)))
    (cons x xs)))

(define (try-nat)
  (let ((xs (try-some try-digit)))
    (string->number (list->string xs))))

(define (try-int)
  (try-choice
   (thunk (let ((_ (try-char #\-))
                (n (try-nat)))
            (- n)))
   try-nat))

(define (try-space)
  (let ((_ (try-many (thunk (try-sat char-whitespace?)))))
    'space))

(define (try-space+)
  (let ((_ (try-some (thunk (try-sat char-whitespace?)))))
    'space))


;; Tests
(module+ test
  (require rackunit rackunit/text-ui)
  (require syntax/parse/define)

  (define-syntax-parser match?
    [(match? pattern:expr subject:expr)
     #'(match subject
         [pattern #t]
         [_ #f])]
    [(match? pattern:expr)
     #'(match-lambda
         [pattern #t]
         [_ #f])])

  (define (parse-test try-proc inp)
    (with-handlers ([exn:fail:parsing?
                     (lambda (exn)
                       (list #f unconsumed-inp))])
      (list #t (parse try-proc inp) unconsumed-inp)))

  (define parsing-tests
    (test-suite
     "Tests for parsing"
     
     (check-equal? (parse-test try-item '()) '(#f ()))
     (check-equal? (parse-test try-item (string->list "abc")) '(#t #\a (#\b #\c)))

     (check-equal? (parse-test (compose char-upcase try-item) (string->list "abc")) '(#t #\A (#\b #\c)))
     (check-equal? (parse-test (compose char-upcase try-item) '()) '(#f ()))
     (check-equal? (parse-test (const 1) (string->list "abc")) '(#t 1 (#\a #\b #\c)))

     (let ()
       (define (try-three)
         (define (g x y z)
           (list x z))
         (g (try-item) (try-item) (try-item)))
       
       (check-equal? (parse-test try-three (string->list "abcdef")) '(#t (#\a #\c) (#\d #\e #\f)))
       (check-equal? (parse-test try-three (string->list "ab")) '(#f ()))
       )

     (let ()
       (define (try-three-by-do)
         (let ((x (try-item))
               (_  (try-item))
               (z (try-item)))
           (list x z)))
       
       (check-equal? (parse-test try-three-by-do (string->list "abcdef")) '(#t (#\a #\c) (#\d #\e #\f)))
       (check-equal? (parse-test try-three-by-do (string->list "ab")) '(#f ())) )

     (check-equal? (parse-test try-empty (string->list "abc")) '(#f (#\a #\b #\c)))
     (check-equal? (parse-test (thunk (try-choice try-item (const #\d))) (string->list "abc")) '(#t #\a (#\b #\c)))
     (check-equal? (parse-test (thunk (try-choice try-empty (const #\d))) (string->list "abc")) '(#t #\d (#\a #\b #\c)))

     (check-equal? (parse-test (thunk (try-choice (thunk (try-char #\a))
                                                  (thunk (try-char #\b)))) (string->list "bbc")) '(#t #\b (#\b #\c)))
     (check-equal? (parse-test (thunk (try-choice (thunk (try-char #\a))
                                                  (thunk (try-char #\b))
                                                  (thunk (try-char #\c)) )) (string->list "cbc")) '(#t #\c (#\b #\c)))
     (check-equal? (parse-test (thunk (try-choice (thunk (try-char #\a))
                                                  (thunk (try-char #\b))
                                                  (thunk (try-char #\c))
                                                  (thunk (try-char #\d)) )) (string->list "dbc")) '(#t #\d (#\b #\c)))
     (check-equal? (parse-test (thunk (try-choice (thunk (try-char #\a))
                                                  (thunk (try-char #\b))
                                                  (thunk (try-char #\c))
                                                  (thunk (try-char #\d))
                                                  (thunk (try-char #\e)))) (string->list "ebc")) '(#t #\e (#\b #\c)))
     (check-equal? (parse-test (thunk (try-choice (thunk (try-char #\a))
                                                  (thunk (try-char #\b))
                                                  (thunk (try-char #\c))
                                                  (thunk (try-char #\d))
                                                  (thunk (try-char #\e)))) (string->list "cbc")) '(#t #\c (#\b #\c)))
     
     (check-equal? (parse-test (thunk (try-char #\a)) (string->list "abc")) '(#t #\a (#\b #\c)))
     (check-equal? (parse-test (thunk (try-chars (string->list "abc"))) (string->list "abceef")) '(#t (#\a #\b #\c) (#\e #\e #\f)))
     (check-equal? (parse-test (thunk (try-chars (string->list "abc"))) (string->list "ab1234")) '(#f (#\2 #\3 #\4)))
     (check-equal? (parse-test try-ident (string->list "abc def")) '(#t (#\a #\b #\c) (#\space #\d #\e #\f)))
     (check-equal? (parse-test try-nat (string->list "123 abc")) '(#t 123 (#\space #\a #\b #\c)))
     (check-equal? (parse-test try-int (string->list "-123 abc")) '(#t -123 (#\space #\a #\b #\c)))
     (check-equal? (parse-test try-space (string->list " abc")) '(#t space (#\a #\b #\c)))

     (let ((comment-head "--"))
       (define (try-comment)
         (try-chars (string->list comment-head))
         (try-many (thunk (try-sat (λ (x) (not (equal? #\newline x))))))
         'comment)

       (define (try-junk)
         (try-many (thunk (try-choice try-space+ try-comment)))
         'junk)
       
       (define (try-token try-p)
         (let* ((v (try-p))
                (_ (try-junk)))
           v))

       (define (try-identifier)
         (try-token try-ident))

       (define (try-natural)
         (try-token try-nat))

       (define (try-integer)
         (try-token try-int))

       (define (try-symbol xs)
         (try-token (thunk (try-chars xs))))

       (define (try-nats)
         (let* ((_ (try-junk))
                (_ (try-symbol (string->list "[")))
                (n (try-natural))
                (ns (try-many (thunk
                               (try-symbol (string->list ","))
                               (try-natural))))
                (_ (try-symbol (string->list "]"))))
           (cons n ns)))
       
       (check-equal? (parse-test try-nats (string->list " [1, 2, 3]")) '(#t (1 2 3) ()))
       (check-equal? (parse-test try-nats (string->list "[1,2,]")) '(#f (#\])))
       )
     ))

  (run-tests parsing-tests)
  )
