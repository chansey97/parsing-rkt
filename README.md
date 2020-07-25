# parsing-rkt
An backtracking parser combinator for Racket



## Introduction

This repository contains the following stuff.

1. A parser combinator library 
   `./parsing.rkt`
2. A simple arithmetic expression parser and evaluator 
   `./examples/arithmetic.rkt`
3. A s-expression parser 
   `./examples/s-expr.rkt`
4. A scheme parser which following r5rs
   `./examples/scheme/parser.rkt`



## How to use

See examples and tests in code.

Keep in mind, use `let*` for sequence, `try-choice` for choice and `try-not-followed-by` for checking follow-set.


## Limitations

1. Not support non-backtracking behaviour, e.g. `<|>` in `parsec`.

   Therefore, it may have some performance issue and in some case it may not be able to give you precise error reports.

2. Not support merge error, e,g. `mergeError` in `parsec`. 

   Therefore, it may not be able to give you precise error reports.
