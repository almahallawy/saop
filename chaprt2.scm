;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 2 - Procedures and Recursions

;;; 2.2 Procedures

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
    (make-list-of-two (car ls) (car (cdr ls)))))

(define second-group
  (lambda (ls)
    (cdr (cdr ls))))

(regroup '(chicken soup ice cream))

(list 'a 'b'c 'd)
(list '(1 2) '(3 4))
(list)


;;;; Exercise 2.1: second
(define second
  (lambda (ls)
    (car (cdr ls))))

(second '(1 2 3 4))

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
(firsts-of-both '((a b) (c d)) '((e f) (g h)))
;;==>((a b) (e f))


;;;; Exercise 2.4: Juggle
(define juggle
  (lambda (list-of-3)
    (cons (third list-of-3)
		      (make-list-of-two (car list-of-3)
					(second list-of-3)))))

(juggle '(1 2 3))
;;=> (3 1 2)
(juggle '(jump quick spot))
;;=>(spot jump quick)
(juggle '(dog bites man))
;;=>(man dog bites)

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


;;; Conditional Expressions
