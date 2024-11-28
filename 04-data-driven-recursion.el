;; -*- mode: Lisp;-*-

;; Chapter 4: Data Driven Recursion

;;; 4.2 Flat Recursion
(defun append (ls1 ls2)
  (if (null ls1)
      ls2
    (cons (car ls1)
	  (append (cdr ls1) ls2))))

(append '(a b c) '(c d))
(append '() '(a b c))


(defun reverse (ls)
  (if (null ls)
      nil
    (append (reverse (cdr ls))
	    (list (car ls)))))

(reverse '(1 2 3 4 5))
(reverse '((1 2) (3 4) (5 6)))


(defun merge (sntp1 sntp2)
  (cond
   ((null sntp1) sntp2)
   ((null sntp2) sntp1)
   ((< (car sntp1) (car sntp2))
    (cons (car sntp1)
	  (merge (cdr sntp1) sntp2)))
   (t (cons (car sntp2)
	    (merge sntp1 (cdr sntp2))))))

(merge '(2.3 4.7 5 8.1) '(1.7 4.7))

(defun even? (int)
  (if (zerop int)
      t
    (odd? (1- int))))

(defun odd? (int)
  (if (zerop int)
      nil
    (even? (1- int))))

(even? 10)
(even? 9)
(odd? 10)
(odd? 9)
(odd? 0)
(even? 0)

(defun remove (item ls)
  (cond
   ((null ls) '())
   ((equal (car ls) item)
    (remove item (cdr ls)))
   (t (cons (car ls)
	    (remove item (cdr ls))))))


;;;; Exercise 4.1: insert-left
(defun insert-left (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons new (cons old (insert-left new old (cdr ls)))))
   (t (cons (car ls)
	    (insert-left new old (cdr ls))))))

(insert-left 'z 'a '(a b a c a))
(insert-left 0 1 '(0 1 0 1))
(insert-left 'dog 'cat '(my dog is fun))
(insert-left 'two 'one '())

;;;; Exercise 4.2: insert-right
(defun insert-right (new old ls)
  (cond
   ((null ls) ls)
   ((equal (car ls) old)
    (cons old (cons new (insert-right new old (cdr ls)))))
   (t (cons (car ls)
	    (insert-right new old (cdr ls))))))

(insert-right 'z 'a '(a b a c a))
(insert-right 0 1 '(0 1 0 1))
(insert-right 'dog 'cat '(my dog is fun))
(insert-right 'two 'one '())


;;;; Exercise 4.3: subst
(defun subst (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons new (subst new old (cdr ls))))
   (t (cons (car ls)
	    (subst new old (cdr ls))))))


(subst 'z 'a '(a b a c a))
(subst 0 1 '(0 1 0 1))
(subst 'dog 'cat '(my dog is fun))
(subst 'two 'one '())


(defun deepen-1 (ls)
  (cond
   ((null ls) '())
   (t (cons (list (car ls))
	    (deepen-1 (cdr ls))))))

(deepen-1 '(a b c))
(deepen-1 '((a b) (c (d e)) f))
(deepen-1 '())


;;; 4.3 Deep Recursion

;;As defined in the little schemer page xii
(defun atom? (x)
  (not (listp x)))

(atom? '())
(atom? 'sadf)

(defun count-all (ls)
  (cond
   ((null ls) 0)
   ((not (consp (car ls)))
    (1+ (count-all (cdr ls))))
   (t (+ (count-all (car ls))
	 (count-all (cdr ls))))))

(defun cound-all (ls)
  (cond
   ((null ls) 0)
   (t (+ (if (consp (car ls))
	     (count-all (car ls))
	   1)
	 (count-all (cdr ls))))))


(count-all '((a b) c () ((d (e)))))
(count-all '(() () ()))
(count-all '((())))
(count-all '())


(defun remove-all (item ls)
  (cond
   ((null ls) '())
   ((equal (car ls) item)
    (remove-all item (cdr ls)))
   (t (cons (if (consp (car ls))
		 (remove-all item (car ls))
	       (car ls))
	     (remove-all item (cdr ls))))))

(remove-all 'a '((a b (c a)) (b (a c) a)))

(defun remq-all (item ls)
  (cond
   ((null ls) '())
   ((consp (car ls))
    (cons (remq-all item (car ls))
	  (remq-all item (cdr ls))))
   ((eq (car ls) item)
    (remq-all item (cdr ls)))
   (t (cons (car ls)
	    (remq-all item (cdr ls))))))

(remq-all 'a '((a b (c a)) (b (a c) a)))

(reverse '(a (b c) (d (e f))))

(defun reverse-all (ls)
  (if (null ls)
      '()
    (append (reverse-all (cdr ls))
	    (list (if (consp (car ls))
		      (reverse-all (car ls))
		    (car ls))))))

(reverse-all '(a (b c) (d (e f))))

;;;; Exercise 4.5: subst-all, substq-all

(defun subst-all (new old ls)
  (cond
   ((null ls) '())
   ((equal (car ls) old)
    (cons new (subst-all new old (cdr ls))))
   (t (cons (if (consp (car ls))
		(subst-all new old (car ls))
	      (car ls))
	    (subst-all new old (cdr ls))))))

(subst-all 'z 'a '(a (b (a c)) (a (d a))))
(subst-all 0 '(1) '(((1) (0))))
(subst-all 'one 'two '())

(defun substq-all (new old ls)
  (cond
   ((null ls) '())
   ((consp (car ls))
    (cons (substq-all new old (car ls))
	  (substq-all new old (cdr ls))))
   (t (cons (if (eq (car ls) old)
		new
	      (car ls))
	    (substq-all new old (cdr ls))))))


(substq-all 'z 'a '(a (b (a c)) (a (d a))))
(substq-all 0 '(1) '(((1) (0))))
(substq-all 'one 'two '())

;;;; Exercise 4.6: insert-left-all

(defun insert-left-all (new old ls)
  (cond
   ((null ls) '())
   ((consp (car ls))
    (cons (insert-left-all new old (car ls))
	  (insert-left-all new old (cdr ls))))
   ((eq (car ls) old)
    (cons new (cons old (insert-left-all new old (cdr ls)))))
   (t (cons (car ls)
	    (insert-left-all new old (cdr ls))))))


(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())


;;;; Exercise 4.7: sum-all
(defun sum-all (ls)
  (cond
   ((null ls) 0)
   ((consp (car ls))
    (+ (sum-all (car ls))
       (sum-all (cdr ls))))
   (t (+ (car ls)
	 (sum-all (cdr ls))))))


(sum-all '((1 3) (5 7) (9 11)))
(sum-all '(1 (3 (5 (7 (9))))))
(sum-all '())

;; 4.4 Tree Representation of Lists

(defun depth (item)
  (if (not (consp item))
      0
    (max (1+ (depth (car item)))
	 (depth (cdr item)))))

(depth '(a (b c d) ((e f) g)))


(defun flatten (ls)
  (cond
   ((null ls) '())
   ((consp (car ls))
    (append (flatten (car ls))
	    (flatten (cdr ls))))
   (t (cons (car ls)
	    (flatten (cdr ls))))))

(flatten '(a (b c d) ((e f) g)))


(defun remove-leftmost (item ls)
  (cond
   ((null ls) '())
   ((equal (car ls) item) (cdr ls))
   ((not (consp (car ls)))
    (cons (car ls) (remove-leftmost item (cdr ls))))
   ((member-all? item (car ls))
    (cons (remove-leftmost item (car ls)) (cdr ls)))
   (t (cons (car ls)
	    (remove-leftmost item (cdr ls))))))

(defun member-all? (item ls)
  (cond
   ((null ls) nil)
   ((equal (car ls) item) t)
   ((consp (car ls))
    (or (member-all? item (car ls))
	(member-all? item (cdr ls))))
   (t (member-all? item (cdr ls)))))

(member-all? 'b '(a (b c) (c (b a))))
(member-all? '(c d) '((a (b c)) ((c d) e)))

(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))

;;;;Ex4.8: count-parens-all

(listp '())
(defun count-parens-all (ls)
  (cond
   ((null ls) 2)
   ((listp (car ls))
    (+ (count-parens-all (car ls))
       (count-parens-all (cdr ls))))
   (t (count-parens-all (cdr ls)))))

(count-parens-all '())
(count-parens-all '((a b) c))
(count-parens-all '(((a () b) c) () ((d) e)))
(count-parens-all '(() ()))

;;;; Ex4.9: count-background-all

(consp '())
(defun count-background-all (item ls)
  (cond
   ((null ls) 0)
   ((consp (car ls))
    (+ (count-background-all item (car ls))
       (count-background-all item (cdr ls))))
   ((eq (car ls) item)
    (count-background-all item (cdr ls)))
   (t (1+ (count-background-all item (cdr ls))))))

(count-background-all 'a '((a) b (c a) d))
(count-background-all 'a '((((b (((a)) c))))))
(count-background-all 'b '())

;;;;Ex 4.10

(defun leftmost (ls)
  (cond
   ((null ls) '())
   ((consp (car ls)) (leftmost (car ls)))
   (t (car ls))))

(leftmost '((a b) (c (d e))))
(leftmost '((((c ((e f) g) h)))))
(leftmost '(() a))

;;;; Ex4.11 rightmost
(defun rightmost (ls)
  (cond
   ((null ls) '())
   ((consp (cdr ls)) (rightmost (cdr ls)))
   ((consp (car ls)) (rightmost (car ls)))
   (t (car ls))))

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v))
(rightmost '(((((b (c)))))))
(rightmost '(a ()))

;; 4.5 Numerical Recursion and Iteration
(defun fact (n)
  (if (zerop n)
      1
    (* n (fact (1- n)))))

(fact 5)
(fact 3)


(defun fact-it (n acc)
  (if (zerop n)
      acc
    (fact-it (1- n) (* acc n))))

(fact-it 5 1)

(defun fact (n)
  (fact-it n 1))

(fact 3)


;;; Ex4.13

(defun harmonic-sum (n)
  (cond
   ((zerop n) 0)
   (t (+ (/ 1.0 n)
	 (harmonic-sum (1- n))))))


(harmonic-sum 3)
(harmonic-sum 4)
;; n    acc
;; 3    0
;; 2    1/3
;; 1    1/3 + 1/2
;; 0    1/3 + 1/2 + 1/1

(defun harmonic-sum-it (n acc)
  (if (zerop n)
      acc
    (harmonic-sum-it (1- n)
		     (+ (/ 1.0 n) acc))))

(harmonic-sum-it 3 0)
(harmonic-sum-it 4 0)



;; 4.6 Analyzing the Fibonacci Algorithm
(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)


(defun fib-it (n acc1 acc2)
  (if (= n 1)
      acc2
    (fib-it (1- n) acc2 (+ acc1 acc2))))

(defun fib (n)
  (if (zerop n)
      0
    (fib-it n 0 1)))
(fib-it 6 0 1)
(fib 6)


(defun reverse-it (ls acc)
  (if (null ls)
      acc
    (reverse-it (cdr ls) (cons (car ls) acc))))

(defun reverse (ls)
  (reverse-it ls '()))

(reverse '(1 2 4))

;;; Check exercise solution from scheme implementation.


;;Ex4.15
(defun fib (n)
  (message "n = %d" n)
  (if (< n 2)
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0)
(fib 1)
(fib 4)
(fib 5)

;;Ex4.16
(defun fib-it (n acc1 acc2)
  (message "n = %d, acc1 = %d, acc2 = %d" n acc1 acc2)
  (if (= n 1)
      acc2
    (fib-it (1- n) acc2 (+ acc1 acc2))))

(fib-it 4 0 1)

;;Ex4.17
(defun call-fib (n)
  (1+ (* 2 (1- (fib (1+ n))))))

(defun adds-fib (n)
  (1- (fib (1+ n))))

(call-fib 0)
(adds-fib 0)
(call-fib 1)
(adds-fib 1)
(call-fib 2)
(adds-fib 2)
(call-fib 3)
(adds-fib 3)
(call-fib 4)
(adds-fib 4)
(call-fib 5)
(adds-fib 5)

;;Ex 4.18
(defun length-it (ls acc)
  (if (null ls)
      acc
      (length-it (cdr ls) (1+ acc))))


(length-it '(1 2 3 4 5) 0)


;;Ex 4.19
(defun mk-asc-list-of-ints (n acc)
  (if (zerop n)
      acc
    (mk-asc-list-of-ints (1- n) (cons n acc))))

(mk-asc-list-of-ints 6 '())

(defun mk-desc-list-of-ints (n)
  (if (zerop n)
      '()
    (cons n (mk-desc-list-of-ints (1- n)))))

(mk-desc-list-of-ints 6)

;;Ex 4.20

(defun occurs (ls item)
  (cond
   ((null ls) 0) 
   ((equal (car ls) item)
    (1+ (occurs (cdr ls) item)))
   (t (occurs (cdr ls) item))))

(occurs '(a b a c a d) 'a)
(occurs '(b c a (b a) c a) 'a)
(occurs '(b (c d)) 'a)

(defun occurs-it (ls item acc)
  (cond
   ((null ls) acc)
   ((equal (car ls) item)
    (occurs-it (cdr ls) item (1+ acc)))
   (t (occurs-it (cdr ls) item acc))))

(occurs-it '(a b a c a d) 'a 0)
(occurs-it '(b c a (b a) c a) 'a 0)
(occurs-it '(b (c d)) 'a 0)
