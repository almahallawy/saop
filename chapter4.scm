;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 4: Data Driven Recursion

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

