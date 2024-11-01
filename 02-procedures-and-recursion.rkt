; -*- mode: racket; geiser-scheme-implementation: racket -*-

;; Chapter 2 - Procedure and Recursion

#lang racket


(lambda (item) (cons item '())) 

((lambda (item) (cons item '())) 'bit)

((lambda (item) (cons item '())) (* 5 6))



(define make-list-of-one
  (lambda (item)
    (cons item '())))

(make-list-of-one 'bit)

(define make-list-of-two
  (lambda (item1 item2)
    (cons item1 (make-list-of-one item2))))

(make-list-of-two 'one 'two)

(define regroup
  (lambda (list-of-4)
    (make-list-of-two
     (first-group list-of-4)
     (second-group list-of-4))))

(define first-group
  (lambda (ls)
    (make-list-of-two (car ls) (cadr ls))))

(define second-group
  (lambda (ls)
    (cddr ls)))

(regroup '(chicken soup ice cream))

(list 'a 'b'c 'd)
(list '(1 2) '(3 4))
(list)


;;;; Exercise 2.1: second
(define second (ls)
  (car (cdr ls)))

(second '(1 2 3))

;;;; Exercise 2.2: third
(define third
  (lambda (ls)
    (car (cdr (cdr ls)))))

(third '(1 2 3 4 5))


;;;; Exercies 2.3: firsts-of-both
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

(firsts-of-both '(1 3 5 7) '(2 4 6))
;;==>(1 2)
(firsts-of-both '((a b) (c d)) '((e f) (g h)));;==>((a b) (e f))

;;Ex 2.4
(define juggle
  (lambda (ls)
    (cons (third ls)
          (make-list-of-two (car ls) (second ls)))))


(juggle '(jump quick spot))
(juggle '(dog bites man))

;;; Exercise 2.5: switch
(define switch
  (lambda (list-of-3)
    (cons (third list-of-3)
	  (make-list-of-two (second list-of-3)
			    (car list-of-3)))))

(switch '(1 2 3))
(switch '(jump quick spot))
;;=>(spo0t quick jump)
(juggle '(dog bites man))
;;=>(man bites dog)


;;;; Exercise 2.6
(define a #t)
(define b #t)
(define c #t)
(define e #f)
(define f #f)

(and a (or b e))
(or e (and (not f) a c))
(not (or (not a) (not b)))
(and (or a f) (not (or b e)))

;;;; Exercise 2.7
(define expr #t)

(or (symbol? expr) (not (symbol? expr)))
(and (null? expr) (not (null? expr)))
(not (and (or expr #f) (not expr)))



(define s-and-n-list?
  (lambda (ls)
    (and (pair? ls)
	 (symbol? (car ls))
	 (pair? (cdr ls))
	 (number? (cadr ls)))))

(s-and-n-list? '(a 1 b));;==> #t
(s-and-n-list? '(a b 1));;==> #f


(define s-or-n-list?
  (lambda (ls)
    (and (pair? ls)
	 (or (symbol? (car ls))
	     (number? (car lsz))))))

(s-or-n-list? '(a 1 b));;#t
(s-or-n-list? '(a b 1));;#t

;;;; Exercise 2.8
(s-and-n-list? '(2 pair 12 dozen))
(s-and-n-list? '(b 4 u c a j))
(s-and-n-list? '(a ten))
(s-and-n-list? '(a))

;;;; Exercise 2.9
(s-or-n-list? '(b))
(s-or-n-list? '(c 2 m))
(s-or-n-list? '(10 10 10 10))
(s-or-n-list? '())
