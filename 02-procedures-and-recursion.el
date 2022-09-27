; -*- mode: Lisp;-*-

;; Chapter 2 - Procedures and Recursions

(setq lexical-binding t);; important to include this

(defun make-list-of-one (item)
  (cons item '()))

(make-list-of-one 'bit)


(defun make-list-of-two (item1 item2)
  (cons item1 (make-list-of-one item2)))

(make-list-of-two 'one 'two)


(car (cdr '(1 2 4)))
(cadr '(1 2 4))

(defun first-group (ls)
  (make-list-of-two (car ls) (cadr ls)))

(defun second-group (ls)
  (cddr 'ls))

(defun regroup (list-of-4)
  (make-list-of-two
   (first-group list-of-4)
   (second-group list-of-4)))


(list 'a 'b 'c 'd)
(list '(1 2) '(3 4))
(list)

;; Ex2.1: Second
(defun second (ls)
  (cadr ls))

(second '(1 23 4))
;;Ex 2.2: Third
(defun third (ls)
  (caddr ls))

(third '(1 2 3 4))


;; Ex 2.3 firsts-of-both
(defun firsts-of-both (ls1 ls2)
  (make-list-of-two (car ls1) (car ls2)))


(firsts-of-both '(1 3 5 7) '(2 4 6))

(firsts-of-both '((a b) (c d)) '((e f) (g h)))


;; Ex 2.4 Juggle
(defun juggle (ls)
  (cons (third ls)
	(make-list-of-two (car ls)
			  (second ls))))

(juggle '(jump quick spot))
(juggle '(dog bites man))


;;Ex 2.5: Switch
(defun switch (ls)
  (cons (third ls)
	(make-list-of-two (second ls)
			  (car ls))))

(switch '(1 2 3))

(listp '(1. 2))
(numberp 5)
(numberp 'a)
(symbolp 14)




;;; 2.3 Conditional Expression
(defun type-of (item)
  (cond
   ((consp item) 'pair)
   ((null item) 'empty-list)
   ((numberp item) 'number)
   ((symbolp item) 'symbol)
   (t 'some-other-type)))

(type-of '(1 2))
(type-of '())
(type-of 12)
(type-of 'xyz)
(type-of car)
