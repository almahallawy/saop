;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 4: Data Driven Recursion

;;; 4.2 Flat Recursion

(define append
  (lambda (ls1 ls2)
    (if (null? ls1)
	ls2
	(cons (car ls1) (append (cdr ls1) ls2)))))

(append '(a b c) '(c d))
(append '() '(a b c))


(define reverse
  (lambda (ls)
    (if (null? ls)
	ls
	(append (reverse (cdr ls)) (cons (car ls) '())))))

(define reverse
  (lambda (ls)
    (if (null? ls)
	'()
	(append (reverse (cdr ls)) (list (car ls))))))


(reverse '(1 2 3 4 5))
(reverse '((1 2) (3 4) (5 6)))

;; tup1 and tup2 are sorted (use shorter name)
(define merge
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     ((< (car tup1) (car tup2))
      (cons (car tup1)
	    (merge (cdr tup1) tup2)))
     (else
      (cons (car tup2)
	    (merge tup1 (cdr tup2)))))))

(merge '(2.3 4.7 5 8.1) '(1.7 4.7))

(define sub1
  (lambda (n)
    (- n 1)))

(define even?
  (lambda (int)
    (if (zero? int)
	#t
	(odd? (sub1 int)))))

(define odd?
  (lambda (int)
    (if (zero? int)
	#f)
    (even? (sub1 int))))

(even? 10)
(even? 9)
(odd? 10)
(odd? 9)
(odd? 0)
(even? 0)

(define remove
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item)
      (remove item (cdr ls)))
     (else (cons (car ls)
		 (remove item (cdr ls)))))))

;;;; Exercise 4.1: insert-left
(define insert-left
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons new
	    (cons old (insert-left new old (cdr ls)))))
     (else (cons (car ls)
		 (insert-left new old (cdr ls)))))))

(insert-left 'z 'a '(a b a c a))
(insert-left 0 1 '(0 1 0 1))
(insert-left 'dog 'cat '(my dog is fun))
(insert-left 'two 'one '())

;;;; Exercise 4.2: insert-right
(define insert-right
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons old
	    (cons new (insert-right new old (cdr ls)))))
     (else (cons (car ls)
		 (insert-right new old (cdr ls)))))))

(insert-right 'z 'a '(a b a c a))
(insert-right 0 1 '(0 1 0 1))
(insert-right 'dog 'cat '(my dog is fun))
(insert-right 'two 'one '())

;;;; Exercise 4.3: subst
(define subst
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons new (subst new old (cdr ls))))
     (else (cons (car ls)
		 (subst new old (cdr ls)))))))

(subst 'z 'a '(a b a c a))
(subst 0 1 '(0 1 0 1))
(subst 'dog 'cat '(my dog is fun))
(subst 'two 'one '())

;;;; Exercise 4.4: deepen-1
(define deepen-1
  (lambda (ls)
    (if (null? ls)
	'()
	(cons (list (car ls))
	      (deepen-1 (cdr ls))))))

(deepen-1 '(a b c))
(deepen-1 '((a b) (c (d e)) f))
(deepen-1 '())

;;; 4.3 Deep Recursion

(define add1
  (lambda (n)
    (+ n 1)))

(define count-all
  (lambda (ls)
    (cond
     ((null? ls) 0)
     ((not  (pair? (car ls))) (add1  (count-all (cdr ls))))
     (else (+ (count-all (car ls)) (count-all (cdr ls)))))))

(define count-all
  (lambda (ls)
    (cond
     ((null? ls) 0)
     (else (+ (if (pair? (car ls))
		  (count-all (cdr ls))
		  1)
	      (count-all (cdr ls)))))))

(count-all '((a b) c () ((d (e)))))
(count-all '(() () ()))
(count-all '((())))
(count-all '())


(define remove-all
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item)
      (remove-all item (cdr ls)))
     ((pair? (car ls))
      (cons (remove-all item (car ls))
	    (remove-all item (cdr ls))))
     (else (cons (car ls)
		 (remove-all item (cdr ls)))))))

(define remq-all
  (lambda (symbl ls)
    (cond
     ((null? ls) '())
     ((pair? (car ls))
      (cons (remq-all symbl (car ls))
	    (remq-all symbl (cdr ls))))
     ((eq? (car ls) symbl)
      (remq-all symbl (cdr ls)))
     (else (cons (car ls)
		 (remq-all symbl (cdr ls)))))))

(remove-all 'a '((a b (c a)) (b (a c) a)))
(remq-all 'a '((a b (c a)) (b (a c) a)))


(reverse '(a (b c) (d (e f))))

(define reverse-all
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((pair? (car ls))
      (append (reverse-all (cdr ls))
	      (list(reverse-all (car ls)))))
     (else (append (reverse-all (cdr ls))
		   (list (car ls)))))))

(define reverse-all
  (lambda (ls)
    (if (null? ls)
	'()
	(append (reverse-all (cdr ls))
		(list (if (pair? (car ls))
			  (reverse-all (car ls))
			  (car ls)))))))

(reverse-all '(a (b c) (d (e f))))


;;;; Exercise 4.5: subst-all, substq-all

(define subst-all
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons new (subst-all new old (cdr ls))))
     ((pair? (car ls))
      (cons (subst-all new old (car ls))
	    (subst-all new old (cdr ls))))
     (else (cons (car ls) (subst-all new old (cdr ls)))))))

(subst-all 'z 'a '(a (b (a c)) (a (d a))))
(subst-all 0 '(1) '(((1) (0))))
(subst-all 'one 'two '())


(define substq-all
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((eq? (car ls) old)
      (cons new (substq-all new old (cdr ls))))
     ((pair? (car ls))
      (cons (substq-all new old (car ls))
	    (substq-all new old (cdr ls))))
     (else (cons (car ls) (substq-all new old (cdr ls)))))))

(substq-all 'z 'a '(a (b (a c)) (a (d a))))
(substq-all 0 '(1) '(((1) (0))))
(substq-all 'one 'two '())

;;;; Exercise 4.6: insert-left-all
(define insert-left-all
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((pair? (car ls))
      (cons (insert-left-all new old (car ls))
	    (insert-left-all new old (cdr ls))))
     ((equal? (car ls) old)
      (cons new (cons old
		      (insert-left-all new old (cdr ls)))))
     (else (cons (car ls)
		 (insert-left-all new old (cdr ls)))))))

(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())


;;;; Exercise 4.7: sum-all

(define sum-all
  (lambda (ls)
    (cond
     ((null? ls) 0)
     ((pair? (car ls))
      (+ (sum-all (car ls))
	 (sum-all (cdr ls))))
     (else (+ (car ls)
	      (sum-all (cdr ls)))))))

(sum-all '((1 3) (5 7) (9 11)))
(sum-all '(1 (3 (5 (7 (9))))))
(sum-all '())


;; 4.4 Tree Representation of Lists
(define depth
  (lambda (item)
    (if (not (pair? item))
	0
	(max (add1 (depth (car item)))
	     (depth (cdr item))))))

(depth '(a (b c d) ((e f) g)))

(define flatten
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((pair? (car ls))
      (append (flatten (car ls))
	      (flatten (cdr ls))))
     (else (cons (car ls) 
		   (flatten (cdr ls)))))))


(flatten '(a (b c d) ((e f) g)))


(define remove-leftmost
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     ((not (pair? (car ls)))
      (cons (car ls) (remove-leftmost item (cdr ls))))
     ((member-all? item (car ls))
      (cons (remove-leftmost item (car ls)) (cdr ls)))
     (else (cons (car ls)
		 (remove-leftmost item (cdr ls)))))))

(define member-all?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     ((equal? (car ls) item) #t)
     ((pair? (car ls))
      (or (member-all? item (car ls))
	  (member-all? item (cdr ls))))
     (else (member-all?  item (cdr ls))))))

(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))
 
;;;; Ex4.8: count-parens-all

;; (count-parens-all '()) = 2
;; (count-parens-all ls) =  (count-parens-all (cdr ls)) if (car ls) is atom
;; (count-parens-all ls) = (count-parens-all (car ls)) + (count-parens-all (cdr ls)) if (car ls) is list 


(define count-parens-all
  (lambda (ls)
    (cond
     ((null? ls) 2)
     ((pair? (car ls)) (+ (count-parens-all (car ls))
			  (count-parens-all (cdr ls))))
     ((null? (car ls)) (+ 2 (count-parens-all (cdr ls))))
     (else (count-parens-all (cdr ls))))))

(count-parens-all '())
(count-parens-all '((a b) c))
(count-parens-all '(((a () b) c) () ((d) e)))
(count-parens-all '(() ()))


;;;; Ex 4.9: count-background-all
(define count-background-all
  (lambda (item ls)
    (cond
     ((null? ls) 0)
     ((pair? (car ls))
      (+ (count-background-all item (car ls))
	 (count-background-all item (cdr ls))))
     ((eq? (car ls) item)
      (count-background-all item (cdr ls)))
     (else (1+ (count-background-all item (cdr ls)))))))

(count-background-all 'a '((a) b (c a) d))
(count-background-all 'a '((((b (((a)) c))))))
(count-background-all 'b '())

;;;; Ex4.10: leftmost
(define leftmost
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((pair? (car ls)) (leftmost (car ls)))
     (else (car ls)))))

(leftmost '((a b) (c (d e))))
(leftmost '((((c ((e f) g) h)))))
(leftmost '(() a))

;;;; Ex4.11: rightmost
(define rightmost
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((pair? (cdr ls)) (rightmost (cdr ls)))
     ((pair? (car ls)) (rightmost (car ls)))
     (else (car ls)))))

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v))
(rightmost '(((((b (c)))))))
(rightmost '(a ()))

;; 4.5 Numerical Recursion and Iteration
(define fact
  (lambda (n)
    (if (zero? n)
	1
	(* n (fact (1- n))))))

(fact 5) 
(fact 3)


(define fact-it
  (lambda (n acc)
    (if (zero? n)
	acc
	(fact-it (1- n) (* n acc)))))

(fact-it 5 1)
(fact-it 3 1)

(define fact
  (lambda (n)
    (fact-it n 1)))

(fact 5)


;;;;Ex4.12
(fact-it 10 1)
(fact 10)

(fact-it 20 1)
(fact 20)

(fact-it 30 1)
(fact 30)


(fact-it 40 1)
(fact 40)

(fact-it 50 1)
(fact 50)

(fact-it 100 1)
(fact 100)

;;;;Ex4.13 harmonic-sum-it

(define harmonic-sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ (/ 1 n) (harmonic-sum (1- n)))))))

(harmonic-sum 3)
(harmonic-sum 4)
;; n    acc
;; 3    0
;; 2    1/3
;; 1    1/3 + 1/2
;; 0    1/3 + 1/2 + 1/1

(define harmonic-sum-it
  (lambda (n acc)
    (if (zero? n)
	acc
	(harmonic-sum-it (1- n) (+ (/ 1 n) acc)))))

(harmonic-sum-it 3 0)
(harmonic-sum-it 4 0)

(harmonic-sum-it 100 0)

;; 4.6 Analyzing the Fibonacci Algorithm

(define fib
  (lambda (n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2))))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)


(define fib-it
  (lambda (n acc1 acc2)
    (if (= n 1)
	acc2
	(fib-it (1- n) acc2 (+ acc1 acc2)))))

(define fib
  (lambda (n)
    (if (zero? n)
	0
	(fib-it n 0 1))))

(fib-it 6 0 1)
(fib 6)


(define reverse-it
  (lambda (ls acc)
    (if (null? ls)
	acc
	(reverse-it (cdr ls) (cons (car ls) acc)))))

(define reverse
  (lambda (ls)
    (reverse-it ls '())))

(reverse '(1 2 4))



;;;; EX4.15

;; - Program 7.5, pg. 199 -
(define writeln
  (lambda args
    (for-each display args)
    (newline)))

(define fib
  (lambda (n)
    (writeln "n = " n)
    (if (< n 2)
	n
	(+ (fib (- n 1))
	   (fib (- n 2))))))

(fib 4)
;; => 3

;; n = 4
;; n = 3
;; n = 2
;; n = 1
;; n = 0
;; n = 1
;; n = 2
;; n = 1
;; n = 0

(fib 5)
(fib 6)


;;;;Ex 4.16
(define fib-it
  (lambda (n acc1 acc2)
    (writeln "n = " n ", acc1 = " acc1 ", acc2 - " acc2)
    (if (= n 1)
	acc2
	(fib-it (1- n) acc2 (+ acc1 acc2)))))

(fib-it 4 0 1)

;; => 3

;; n = 4, acc1 = 0, acc2 - 1
;; n = 3, acc1 = 1, acc2 - 1
;; n = 2, acc1 = 1, acc2 - 2
;; n = 1, acc1 = 2, acc2 - 3



;;;;Ex 4.17

(define calls-fib
  (lambda (n)
    (1+ (* 2 (1- (fib (1+ n)))))))

(define adds-fib
  (lambda (n)
    (1- (fib (1+ n)))))


(fib 0)
(calls-fib 0)
(adds-fib 0)

(fib 1)
(calls-fib 1)
(adds-fib 1)

(fib 2)
(calls-fib 2)
(adds-fib 2)

(fib 3)
(calls-fib 3)
(adds-fib 3)

(fib 4)
(calls-fib 4)
(adds-fib 4)

(fib 5 )
(calls-fib 5)
 (adds-fib 5)


;;; Ex4.18 length-it

(define length-it
  (lambda (ls acc)
    (if (null? ls)
	acc
	(length-it (cdr ls) (1+ acc)))))



(length-it '(1 2 3 4 ) 0)

;;;; Ex 4.19
(define mk-asc-list-of-ints
  (lambda (n acc)
    (if (zero? n)
	acc
	(mk-asc-list-of-ints (1- n)
			     (cons n acc)))))

(mk-asc-list-of-ints 10 '())


(define mk-desc-list-of-ints
  (lambda (n)
    (reverse (mk-asc-list-of-ints n '()))))


(mk-desc-list-of-ints 6)


;;;; Ex4.20
(define occurs
  (lambda (ls a)
    (cond
     ((null? ls) 0)
     ((eq? (car ls) a)
      (1+ (occurs (cdr ls) a)))
     (else (occurs (cdr ls) a)))))

(occurs '(a b a c a d) 'a)
(occurs '(b c a (b a) c a) 'a)
(occurs '(b (c d)) 'a)


(define occurs-it
  (lambda (ls a acc)
    (cond
     ((null? ls) acc)
     ((eq? (car ls) a)
      (occurs-it (cdr ls) a (1+ acc)))
     (else (occurs-it (cdr ls) a acc)))))


(occurs-it '(a b a c a d) 'a 0)
(occurs-it '(b c a (b a) c a) 'a 0)
(occurs-it '(b (c d)) 'a 0)




