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


;;;; Exercise 2.10
(define last-item
  (lambda (ls)
    (if (null? (cdr ls))
	(car ls)
	(last-item (cdr ls)))))

(last-item '(1 2 3 4 5))
(last-item '(a b (c d)))
(last-item '(cat))
(last-item '((cat)))

(define member?
  (lambda (item ls)
    (if (null? ls)
	#f
	(or (equal? (car ls) item)
	    (member? item (cdr ls))))))

(member? 'cat '(dog hen cat pig))
(member? 'fox '(dog hen cat pig))
(member? 2 '(1 (2 3) 4))
(member? '(2 3) '(1 (2 3) 4))
(member? 'cat '())


(define remove-1st
  (lambda (item ls)
    (if (null? ls)
	'()
	(if (equal? (car ls) item)
	    (cdr ls)
	    (cons (car ls) (remove-1st item (cdr ls)))))))

(remove-1st 'fox '(hen fox chick cock))
(remove-1st 'fox '(hen fox chick fox cock))
(remove-1st 'fox '(hen (fox chick) cock))
(remove-1st 'fox '())
(remove-1st '(1 2) '(1 2 (1 2) ((1 2))))


;;;; Exercise 2.11
(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     ((equal? (car ls) item) #t)
     (else (member? item (cdr ls))))))

;;;; Exercise 2.12

;; ls at least contains two element 
(define mystery
  (lambda (ls)
    (if (null? (cddr ls)) ;; if list has two elements
	(cons (car ls) '()) ;;then return the first elment in list
	(cons (car ls) (mystery (cdr ls)))))) ;;else build list with (car ls) and remaining for mystery(ls)

(mystery '(1 2 3 4 5))
;;=> (1 2 3 4)
(mystery '(1 2 3 4))
;;=> (1 2 3)
(mystery '(1 2 3))
;;=> (1 2)
(mystery '(1 2))
;;=> (1)

;; this list removes the last element. So a good name remove-last



;;;; Exercise 2.13: subst-1st

(define subst-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls)
		 (subst-1st new old (cdr ls)))))))

(subst-1st 'dog 'cat '(my cat is clever))
(subst-1st 'b 'a '(c a b a c))
(subst-1st '(0) '(*) '((*) (1) (*) (2)))
(subst-1st 'two 'one '())

(define substq-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((eq? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls)
		 (subst-1st new old (cdr ls)))))))

(define substv-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((eqv? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls)
		 (subst-1st new old (cdr ls)))))))

;;;; Exercise 2.14: insert-right-1st

(define insert-right-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons old (cons new (cdr ls))))
     (else (cons (car ls)
		 (insert-right-1st new old (cdr ls)))))))

(insert-right-1st 'not 'does '(my dog does have fleas))

(define insert-left-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons new ls))
     (else (cons (car ls)
		 (insert-left-1st new old (cdr ls)))))))


(insert-left-1st 'hot 'dogs '(I eat dogs))
(insert-left-1st 'fun 'games '(some fun))
(insert-left-1st 'a 'b '(a b c a b c))
(insert-left-1st 'a 'b '())

;;;; Exercise 2.15: list-of-first-items

(define list-of-first-items
  (lambda (ls)
    (cond
     ((null? ls) '())
     (else (cons (caar ls)
		 (list-of-first-items (cdr ls)))))))

(list-of-first-items '((a) (b c d) (e f)))
(list-of-first-items '((1 2 3)  (4 5 6)))
(list-of-first-items '((one)))
(list-of-first-items '())

;;;; Exercise 2.16: replace

(define replace
  (lambda (new-item ls)
    (cond
     ((null? ls) '())
     (else (cons new-item (replace new-item (cdr ls)))))))

(replace 'no '(will you do me a favor))
(replace 'yes '(do you like ice cream))
(replace 'why '(not))
(replace 'maybe '())


;;;; Exercise 2.17
(define remove-2nd
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item)
      (cons (car ls) (remove-1st item (cdr ls))))
     (else (cons (car ls) (remove-2nd item (cdr ls)))))))

(remove-2nd 'cat '(my cat loves cat food))
(remove-2nd 'cat '(my cat loves food))
(remove-2nd 'cat '(my cat and your cat loves cat food))
(remove-2nd 'cat '())

;;;; Exercise 2.18

(define remove-last
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item)
      (if (member? item (cdr ls))
	  (cons (car ls) (remove-last item (cdr ls)))
	  (cdr ls)))
     (else (cons (car ls)
		 (remove-last item (cdr ls)))))))

(remove-last 'a '(b a n a n a s))
(remove-last 'a '(b a n a n a))
(remove-last 'a '())

;;;; Exercise 2.19
(define sandwich-1st
  (lambda (a b ls)
    (cond
     ((null? ls) '())
     ((null? (cdr ls)) ls)
     ((equal? (car ls) b)
      (if (equal? (cadr ls) b)
	  (cons (car ls)
		(cons a (cdr ls)))
	  (cons (car ls)
		(sandwich-1st a b (cdr ls)))))
     (else (cons (car ls)
		 (sandwich-1st a b (cdr ls)))))))

(sandwich-1st 'meat 'bread '(bread cheese bread bread))
(sandwich-1st 'meat 'bread '(bread  jam bread cheese bread))
(sandwich-1st 'meat 'bread '())

;;;; Exercise 2.20

(define list-of-symbols?
  (lambda (ls)
    (cond
     ((null? ls) #t)
     ((symbol? (car ls)) (list-of-symbols? (cdr ls)))
     (else #f))))

(define list-of-symbols?
  (lambda (ls)
    (if (null? ls)
	#t
	(if (symbol? (car ls))
	    (list-of-symbols? (cdr ls))
	    #f))))

(define list-of-symbols?
  (lambda (ls)
    (or (null? ls)
	(and (symbol? (car ls))
	     (list-of-symbols? (cdr ls))))))

(list-of-symbols? '(one two three four five))
(list-of-symbols? '(cat dog (hen pig) cow))
(list-of-symbols? '(a b 3 4 d))
(list-of-symbols? '())

;;;; Exercise 2.21
(define all-same?
  (lambda (ls)
    (cond
     ((null? ls) #t)
     ((null? (cdr ls)) #t)
     ((equal? (car ls) (cadr ls))
      (all-same? (cdr ls)))
     (else #f))))

(all-same? '(a a a a a))
(all-same? '(a b a b a b))
(all-same? '((a b) (a b) (a b)))
(all-same? '(a))
(all-same? '())

;;; 2.5 Tracing and Debugging 

(begin
  (write "The remove-1st expression")
  (write "is applied to the list (1 2 3 4)")
  (write "to build a new list without the number 2.")
  (remove-1st 2 '(1 2 3 4)))


