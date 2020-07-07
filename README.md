# parsing-rkt
An imperative style parser combinator for Racket



## Introduction

This repository contains the following stuff.

1. A parser combinator library `./parsing.rkt`
2. A simple arithmetic expression parser and evaluator `examples/arithmetic.rkt`
3. A s-expression parser `examples/s-expr.rkt`
4. A scheme parser which following r5rs `examples/scheme/parser.rkt`



## How to use

See examples and tests in code.

Keep in mind, use `let*` for sequence and `try-choice` for non-deterministic choice.
