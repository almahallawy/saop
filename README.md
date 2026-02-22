# Scheme and the Art of Programming

Code examples and exercises from the book **Scheme and the Art of Programming** (SAOP) by George Springer and Daniel P. Friedman (MIT Press / McGraw-Hill, 1989).

## What is this?

This repository contains implementations of the numbered box programs and selected exercise solutions from each chapter of the SAOP book. The implementations are provided in multiple languages to allow comparison across different Lisp/Scheme dialects:

| File extension | Language |
|---|---|
| `.scm` | Standard Scheme |
| `.rkt` | Racket |
| `.clj` | Clojure |
| `.el` | Emacs Lisp |

The `springer/` directory contains the official original Scheme source code (Copyright © 1990 MIT) for all boxed programs and some exercises, archived from: https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/bookcode/sap/0.html

## Chapters Covered

| File prefix | Chapter |
|---|---|
| `01-data-and-operators` | Chapter 1 – Data and Operators |
| `02-procedures-and-recursion` | Chapter 2 – Procedures and Recursion |
| `03-data-abstraction-and-numbers` | Chapter 3 – Data Abstraction and Numbers |
| `04-data-driven-recursion` | Chapter 4 – Data-Driven Recursion |
| `05-locally-defined-procedures` | Chapter 5 – Locally Defined Procedures |

The `springer/` directory covers Chapters 2–17 of the book.

## How to Run

- **Scheme / Racket (`.rkt`)**: Open in [DrRacket](https://racket-lang.org/) or run with `racket <file>`.
- **Clojure (`.clj`)**: Open in Emacs with [CIDER](https://cider.mx/) (`M-x cider-jack-in`) and evaluate expressions with `C-x C-e`.
- **Emacs Lisp (`.el`)**: Open in Emacs, switch to `lisp-interaction-mode` (`M-x lisp-interaction-mode`), and evaluate expressions in the same buffer with `C-x C-e`. 