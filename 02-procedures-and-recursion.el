; -*- mode: Lisp;-*-

;; Chapter 2 - Procedures and Recursions

(lisp-interaction-mode)
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
;;(type-of car)


(defun car-if-pair (item)
  (cond
   ((consp item) (car item))
   (t item)))

(defun car-if-pair (item)
  (if (consp item)
      (car item)
    item))

(car-if-pair 5)
(car-if-pair '(a b))

(defun s-and-n-list? (ls)
  (and (consp ls)
       (symbolp (car ls))
       (consp (cdr ls))
       (numberp (cadr ls))))

(s-and-n-list? '(a 1 b))
(s-and-n-list? '(a b 1))


(defun s-or-n-list? (ls)
  (and (consp ls)
       (or (symbolp (car ls))
	   (numberp (car ls)))))

(s-or-n-list? '(a 1 b))
(s-or-n-list? '(a b 1))

(defun singleton-list? (ls)
  (and (consp ls) (null (cdr ls))))


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

(defun last-item (ls)
  (cond
   ((null (cdr ls)) (car ls))
   (t (last-item (cdr ls)))))

(last-item '(1 2 3 4 5))
(last-item '(a b (c d)))
(last-item '(cat))
(last-item '((cat)))


(defun member? (item ls)
  (cond
   ((null ls) nil)
   ((equal (car ls) item) t)
   (t (member? item (cdr ls)))))

(member? 'cat '(dog hen cat pig))
(member? 'fox '(dog hen cat pig))
(member? 2 '(1 (2 3) 4))
(member? '(2 3) '(1 (2 3) 4))
(member? 'cat '())


(defun remove-1st (item ls)
  (cond
   ((null ls) nil)
   ((equal (car ls) item) (cdr ls))
   (t (cons (car ls)
	    (remove-1st item (cdr ls))))))

(remove-1st 'fox '(hen fox chick cock))
(remove-1st 'fox '(hen fox chick fox cock))
(remove-1st 'fox '(hen (fox chick) cock))
(remove-1st 'fox '())
(remove-1st '(1 2) '(1 2 (1 2) ((1 2))))


;;Ex 2.10

(defun last-item (ls)
  (if (null (cdr ls))
      (car ls)
    (last-item (cdr ls))))

(defun member? (item ls)
  (if (null ls)
      nil
    (or (equal (car ls) item)
	(member? item (cdr ls)))))

(defun remove-1st (item ls)
  (if (null ls)
      nil
    (if (equal (car ls) item)
	(cdr ls)
      (cons (car ls)
	    (remove-1st item (cdr ls))))))

;;Ex 2.11
(defun mystery (ls)
  (if (null (cddr ls))
      (cons (car ls) '())
    (cons (car ls) (mystery (cdr ls)))))

(mystery '(1 2 3 4 5))

;; mystery = remove-last


;;;; Exercise 2.13: subst-1st
(defun subst-1st (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons new (cdr ls)))
   (t (cons (car ls)
	    (subst-1st new old (cdr ls))))))

(subst-1st 'dog 'cat '(my cat is clever))
(subst-1st 'b 'a '(c a b a c))
(subst-1st '(0) '(*) '((*) (1) (*) (2)))
(subst-1st 'two 'one '())


;;;; Exercise 2.14: insert-right-1t
(defun insert-right-1st (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons (car ls)
	  (cons new (cdr ls))))
   (t (cons (car ls)
	    (insert-right-1st new old (cdr ls))))))

(insert-right-1st 'not 'does '(my dog does have fleas))


(defun insert-left-1st (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons new ls))
   (t (cons (car ls)
	    (insert-left-1st new old (cdr ls))))))


(insert-left-1st 'hot 'dogs '(I eat dogs))
(insert-left-1st 'fun 'games '(some fun))
(insert-left-1st 'a 'b '(a b c a b c))
(insert-left-1st 'a 'b '())


;;;; Exercise 2.15: list-of-first-items
(defun list-of-first-items (ls)
  (cond
   ((null ls) '())
   (t (cons (caar ls)
	    (list-of-first-items (cdr ls))))))

(list-of-first-items '((a) (b c d) (e f)))
(list-of-first-items '((1 2 3)  (4 5 6)))
(list-of-first-items '((one)))
(list-of-first-items '())


;;;; Exercise 2.16: replace
(defun replace (new-item ls)
  (cond
   ((null ls) '())
   (t (cons new-item
	    (replace new-item (cdr ls))))))

(replace 'no '(will you do me a favor))
(replace 'yes '(do you like ice cream))
(replace 'why '(not))
(replace 'maybe '())


;;;; Exercise 2.17
(defun remove-2nd (item ls)
  (cond
   ((null ls) '())
   ((equal (car ls) item)
    (cons (car ls)
	  (remove-1st item (cdr ls))))
   (t (cons (car ls)
	    (remove-2nd item (cdr ls))))))

(remove-2nd 'cat '(my cat loves cat food))
(remove-2nd 'cat '(my cat loves food))
(remove-2nd 'cat '(my cat and your cat loves cat food))
(remove-2nd 'cat '())


;;;; Exercise 2.18
(defun remove-last (item ls)
  (cond
   ((null ls) '())
   ((member? item (cdr ls))
    (cons (car ls)
	  (remove-last item (cdr ls))))
   (t (if (equal (car ls) item)
	  (cdr ls)
	ls))))

(remove-last 'a '(b a n a n a s))
(remove-last 'a '(b a n a n a))
(remove-last 'a '())
(remove-last 'a '(b c d e))
(remove-last 'a '(a b c d e))


;;;; Exercise 2.19

(defun sandwich-1st (a b ls)
  (cond
   ((null ls) '())
   ((equal (car ls) b)
    (cond
     ((null (cdr ls)) ls)
     ((equal (cadr ls) b)
      (cons b
	    (cons a (cdr ls))))
     (t (cons (car ls)
	      (sandwich-1st a b (cdr ls))))))
   (t (cons (car ls)
	    (sandwich-1st a b (cdr ls))))))

(sandwich-1st 'meat 'bread '(bread cheese bread bread))
(sandwich-1st 'meat 'bread '(bread  jam bread cheese bread))
(sandwich-1st 'meat 'bread '())


;;;; Exercise 2.20
(defun list-of-symbols? (ls)
  (cond
   ((null ls) t)
   ((symbolp (car ls))
    (list-of-symbols? (cdr ls)))
   (t nil)))

(list-of-symbols? '(one two three four five))
(list-of-symbols? '(cat dog (hen pig) cow))
(list-of-symbols? '(a b 3 4 d))
(list-of-symbols? '())



;;; Exercise 2.21
(defun all-same? (ls)
  (cond
   ((null ls) t)
   ((null (cdr ls)) t)
   ((equal (car ls) (cadr ls))
    (all-same? (cdr ls)))
   (t nil)))


(all-same? '(a a a a a))
(all-same? '(a b a b a b))
(all-same? '((a b) (a b) (a b)))
(all-same? '(a))
(all-same? '())


;;;; 2.5 Tracing and Debugging

(progn (print 'Punch))



(progn (print 'The\ cat\ in)
       (print "the hat")
       (print " came back"))

(progn
  (print "The remove-1st expression")
  (print  "is applied to the list (1 2 3 4)")
  (print "to build a new list without the number 2.")
  (remove-1st 2 '(1 2 3 4)))

(progn
  (+ 3 4)
  (- 5 11)
  (* 10 10))


(defun remove-1st-trace (item ls)
  (cond
   ((entering (null ls) ls 1)
    (leaving '() 1))
   ((entering (equal (car ls) item) ls 2)
    (leaving (cdr ls) 2))
   (t (entering 'else ls 3)
    (leaving
     (cons (car ls) (remove-1st-trace item (cdr ls)))
     3))))

(defun entering (test input cond-clause-number)
  (progn
    (if test (message "   Entering cond-clause-%d with ls = %s"
		      cond-clause-number  input ))
    test))


(defun leaving (result cond-clause-number)
  (progn
    (message "Leaving cond-clause-%d with result = %s"
	     cond-clause-number result)
    result))


(remove-1st-trace 'c '(a b c d))
(remove-1st-trace 'e '(a b c d))


(defun swapper (x y ls)
  (cond
   ((null ls) '())
   ((equal (car ls) x)
    (cons y (swapper x y (cdr ls))))
   ((equal (car ls) y)
    (cons x (swapper x y (cdr ls))))
   (t (cons (car ls)
	    (swapper x y (cdr ls))))))


(defun swapper (x y ls)
  (cond
   ((null ls) '())
   (t (cons (swap-tester x y (car ls))
	    (swapper x y (cdr ls))))))

(defun swap-tester (x y a)
  (cond
   ((equal a x) y)
   ((equal a y) x)
   (t a)))



(defun swapper (x y ls)
  (cond
   ((null ls) '())
   (t (cons (cond
	     ((equal (car ls) x) y)
	     ((equal (car ls) y) x)
	     (t (car ls)))
	    (swapper x y (cdr ls))))))

(swapper 'cat 'dog '(my cat eats dog food))
(swapper 'john 'mary '(john loves mary))
(swapper 'a 'b  '(c (a b) d))
(swapper 'a 'b '())
(swapper 'b 'd '(a b c d b))


(quote '())
(list 'a 'b)
;;;Ex 2.24

(defun describe (s)
  (cond
   ((null s) (quote '()))
   ((numberp s) s)
   ((symbolp s) (list 'quote s))
   ((consp s) (list 'cons (describe (car s)) (describe (cdr s))))
   (t s)))

(describe 347)
;; => 347
(describe 'hello)
;; => (quote hello)
(describe '(a))
;; => (cons (quote a) (quote ()))
(describe '(a b))
;; => (cons (quote a) (cons (quote b) (quote ())))
(describe '(1 2 button my shoe))
;; => (cons 1 (cons 2 (cons (quote button) (cons (quote my) (cons (quote shoe) (quote ()))))))
(describe '(a (b c (d e) f g) h))
;; => (cons (quote a) (cons (cons (quote b) (cons (quote c) (cons (cons (quote d) (cons (quote e) (quote ()))) (cons (quote f) (cons (quote g) (quote ())))))) (cons (quote h) (quote ()))))


;;Ex2.25
;; use the same entering and leaving procedures above
(defun swapper-trace (x y ls)
  (cond
   ((entering (null ls) ls 1)
    (leaving '() 1))
   ((entering (equal (car ls) x) ls 2)
    (leaving (cons y (swapper-trace x y (cdr ls))) 2))
   ((entering (equal (car ls) y) ls 3)
    (leaving (cons x (swapper-trace x  y (cdr ls))) 3))
   (t (entering 'else ls 4)
      (leaving (cons (car ls) (swapper-trace x y (cdr ls))) 4))))

(swapper-trace 'b 'd '(a b c d b))


;;Ex2.28
(defun tracing (msg result)
  (progn
    (princ msg)
    (princ result)
    (princ "\n")
    result))

(defun test-tracing (test msg input)
  (progn
    (if test (tracing msg input))
    test))


(defun remove-1st-trace (item ls)
  (cond
   ((test-tracing (null ls) "   Entering cond-clause-1 with ls = " ls)
    (tracing "Leaving cond-clause-1 with result = " '()))
   ((test-tracing (equal (car ls) item) "   Entering cond-clause-2 with ls = " ls)
    (tracing "Leaving cond-clause-2 with result = " (cdr ls)))
   (t (test-tracing 'else "   Entering cond-clause-3 with ls = " ls)
    (tracing "Leaving cond-clause-3 with result = "
	       (cons (car ls) (remove-1st-trace item (cdr ls)))))))

(remove-1st-trace 'c '(a b c d))
