; -*- mode: racket; geiser-scheme-implementation: racket -*-

;; Chapter 1 - Data and Operators

#lang racket


(define ten 10)
ten

(quote ten)

(define Robert 'Bob)
Robert

(cons 1 '())
(cons 1 null)

(define ls1 (cons 1 '()))
ls1

(cons 2 ls1)
ls1

(define ls2 (cons 2 ls1))
ls2
ls1

(define c 'three)
(cons c ls2)

(define ls3 (cons c ls2))
ls3

(cons ls2 ls3)
ls3

(define ls4 (cons ls2 ls3))
ls4

(car '(1 2 3 4))

(car ls4)

(car '(()))

(cdr '(1 2 3 4))
(cdr ls4)

;; Pair
(cons 'a 'b)

'(a . ())
'(a . (b c))

(number? -45.67)

(number? (car '(15.3 -31.7)))
(number? (cdr '(15.3 -31.7)))

(define num 35.4)
(define twelve 'dozen)

(number? num)
(number? twelve)

(symbol? num)
(symbol? 'num)
(symbol? twelve)
(symbol? 'twelve)
(symbol? false)
(symbol? (car '(banana cream)))
(symbol? (cdr '(banana cream)))
(symbol? 5) 


(boolean? #t)
(boolean? #f)
(boolean? (number? 'a))
(boolean? (cons 'a '()))

(null? '())
(null? (cdr '(cat)))
(null? (car '((a b))))


(procedure? cons)
(procedure? +)
(procedure? 'cons)
(procedure? 100)


(= 3 (/ 6 2))
(= (/ 12 2) (* 2 3))
(= (car '(-1 ten 543)) (/ -20 (* 4 5)))
(= (* 2 100) 20)


(define Garfield 'cat)
(eq? 'cat 'cat)
(eq? Garfield 'cat)
(eq? Garfield Garfield)
(eq? 'Garfield 'cat)
(eq? (first '(Garfield cat)) 'cat)
(eq? (first '(Garfield cat)) 'Garfield)
(eq? 5 5)


(define ls-a (cons 1 '(2 3)))
(define ls-b (cons 1 '(2 3)))
(define ls-c ls-a)
(eq? (cons 1 '(2 3)) (cons 1 '(2 3)))
(eq? ls-a (cons 1 '(2 3)))
(eq? ls-a ls-b)
(eq? ls-c ls-a)

(eqv? '(1 2) '(1 2))
(eqv? )
