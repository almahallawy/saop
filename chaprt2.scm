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
    (make-list-of-two (car ls) (cadr ls))))

(define second-group
  (lambda (ls)
    (cddr ls)))

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


;;; 2.3 Conditional Expressions

(define type-of
  (lambda (item)
    (cond 
     ((pair? item) 'pair)
     ((null? item) 'empty-list)
     ((number? item) 'number)
     ((symbol? item) 'symbol)
     (else 'some-other-type))))

(type-of '(1 2))
(type-of '())
(type-of 12)
(type-of 'xyz)
(type-of car)

(define car-if-pair
  (lambda (item)
    (cond
     ((pair? item) (car item))
     (else item))))

(car-if-pair 'a)
(car-if-pair '(1 2))

(define car-if-pair
  (lambda (item)
    (if (pair? item)
	(car item)
	item)))

(car-if-pair 5)
(car-if-pair '(a b))

;;replace cond with if. Obscured!!
(define type-of
  (lambda (item)
    (if (pair? item)
	'pair
	(if (null? item)
	    'empty-list
	    (if (number? item)
		'number
		(if (symbol? item)
		    'symbol
		    'some-other-type))))))

(type-of '(1 2))
(type-of '())
(type-of 12)
(type-of 'xyz)
(type-of car)


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
	     (number? (car ls))))))

(s-or-n-list? '(a 1 b));;#t
(s-or-n-list? '(a b 1));;#t


(define singleton-list?
  (lambda (ls)
    (and (pair? ls) (null? (cdr ls)))))

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
(define expr)

(or (symbol? expr) (not (symbol? expr)))
(and (null? expr) (not (null? expr)))
(not (and (or expr #f) (not expr)))



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



;;; 2.4 Recursion
(define last-item
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (last-item (cdr ls))))))

(last-item '(1 2 3 4 5))
(last-item '(a b (c d)))
(last-item '(cat))
(last-item '((cat)))


(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     ((equal? (car ls) item) #t)
     (else (member? item (cdr ls))))))

(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     (else (or (equal? (car ls) item)
	       (member? item (cdr ls)))))))

(member? 'cat '(dog hen cat pig))
(member? 'fox '(dog hen cat pig))
(member? 2 '(1 (2 3) 4))
(member? '(2 3) '(1 (2 3) 4))
(membe? 'cat '())

(memq 'b '(a b c))
(memq 'cat '(dog hen cat pig))
(memq 'fox '(dog hen cat pig))
(memq 'cat '())

(define remove-1st
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     (else (cons (car ls) (remove-1st item (cdr ls)))))))



(remove-1st 'fox '(hen fox chick cock))
(remove-1st 'fox '(hen fox chick fox cock))
(remove-1st 'fox '(hen (fox chick) cock))
(remove-1st 'fox '())
(remove-1st '(1 2) '(1 2 (1 2) ((1 2))))


