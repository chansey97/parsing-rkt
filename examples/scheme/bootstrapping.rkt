#lang racket
(require "../../parsing.rkt")
(require "./parser.rkt")
(require racket/pretty)

(define string-of-scheme-rkt
  (string-replace (file->string "./parser.rkt") "#lang racket" ""))

(define (bootstrapping)
  (define (bootstrapping-loop inp)
    (with-handlers ([exn:fail:parsing?
                     (lambda (exn)
                       (list #f unconsumed-inp))])
      (let ((res (parse (thunk (try-junk) (try-s-expr)) inp)))
        (pretty-print res)
        (bootstrapping-loop (get-unconsumed-inp)))))
  
  (bootstrapping-loop (string->list string-of-scheme-rkt)))

(bootstrapping)
