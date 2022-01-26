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
