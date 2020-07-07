#lang racket
(require "../parsing.rkt")

(define (try-token try-p)
  (let* ((v (try-p))
         (_ (try-space)))
    v))

(define (try-symbol xs)
  (try-token (thunk (try-chars xs))))

(define (try-natural)
  (try-token try-nat))

(define (try-expr)
  (let ((t (try-term)))
    (try-choice
     (thunk
      (let* ((_ (try-symbol (string->list "+")))
             (e (try-expr)))
        (+ t e)))
     (const t))))

(define (try-term)
  (let ((f (try-factor)))
    (try-choice
     (thunk
      (let ((_ (try-symbol (string->list "*")))
            (t (try-term)))
        (* f t)))
     (const f))))

(define (try-factor)
  (try-choice
   (thunk
    (let* ((_ (try-symbol (string->list "(")))
           (e (try-expr))
           (_ (try-symbol (string->list ")"))))
      e))
   try-natural))

(module+ test
  (require rackunit rackunit/text-ui)

  (define (eval str)
    (printf "eval ~v\n" str)
    (define xs (string->list str))

    (with-handlers ([exn:fail:parsing?
                     (lambda (exn)
                       (printf "Invalid input, unused input ~v\n" (get-unconsumed-inp))
                       #f)])
      (let ((n (parse (thunk (try-space) (try-expr)) xs)))
        (match (get-unconsumed-inp)
          ['() n]
          [out (printf "Partial valid input ~v, Unused input ~v\n" n (list->string out)) #f]))))

  (eval "1+2")
  (eval "2*3+4")
  (eval "2*(3+4)")
  (eval "2*(3^4)")
  (eval "one plus two")
  (eval "a")

  ;; eval "1+2"
  ;; 3
  ;; eval "2*3+4"
  ;; 10
  ;; eval "2*(3+4)"
  ;; 14
  ;; eval "2*(3^4)"
  ;; Partial valid input 2, Unused input "*(3^4)"
  ;; #f
  ;; eval "one plus two"
  ;; Invalid input, unused input '(#\n #\e #\space #\p #\l #\u #\s #\space #\t #\w #\o)
  ;; #f
  ;; eval "a"
  ;; Invalid input, unused input '()
  ;; #f
  )



