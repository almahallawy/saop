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

