;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 3 - Data Abstractions and Numbers

(define add1
  (lambda (n)
    (+ n 1)))

(add1 7)
(add1 -37)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 7)
(sub1 -37)


;; - Program 3.4, pg. 77 -

(define harmonic-sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ (/ 1 n) (harmonic-sum (sub1 n)))))))


(define list-of-zeros
  (lambda (n)
    (cond
     ((zero? n) '())
     (else (cons 0 (list-of-zeros (sub1 n)))))))

(list-of-zeros 10)

(define =length=
  (lambda (ls)
    (if (null? ls)
	0
	(add1 (=length= (cdr ls))))))

(=length= '(a b c d e))
(=length= '(1 (2 3) (4 5 6)))
(=length= '())

(define list-ref
  (lambda (ls n)
    (cond
     ((<= (length ls) n)
      (error "list-ref: Index" n "out of range for list" ls))
     ((zero? n) (car ls))
     (else (list-ref (cdr ls) (sub1 n))))))

(define list-ref
  (lambda (ls n)
    (cond
     ((<= (length ls) n)
      (error "list-ref: Index" n "out of range for list" ls))
     (else (list-ref-helper ls n)))))

(define list-ref-helper
  (lambda (ls n)
    (if (zero? n)
	  (car ls)
	  (list-ref-helper (cdr ls) (sub1 n)))))

(define list-ref
  (lambda (ls n)
    (cond
     ((null? ls)
      (error "list-ref: Index" n "out of range for list" ls))
     ((zero? n) (car ls))
     (else (list-ref (cdr ls) (sub1 n))))))

(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)
(list-ref '() 0)


;;;; Exercise 3.1: sum
(define sum
  (lambda (ls)
    (cond
     ((null? ls) 0)
     (else (+ (car ls) (sum (cdr ls)))))))

(sum '(1 2 3 4 5))
(sum '(6))
(sum '())

;;;; Exercise 3.2: pairwise-sum

(define pairwise-sum
  (lambda (ntpl-1 ntpl-2)
    (if (null? ntpl-1)
	'()
	(cons (+ (car ntpl-1) (car ntpl-2))
	      (pairwise-sum (cdr ntpl-1) (cdr ntpl-2))))))

(pairwise-sum '(1 3 2) '(4 -1 2))
(pairwise-sum '(3.2 1.5) '(6.0 -2.5))
(pairwise-sum '(7) '(11))
(pairwise-sum '() '())

(define pairwise-product
  (lambda (tup1 tup2)
    (if (null? tup1)
	'()
	(cons (* (car tup1) (car tup2))
	      (pairwise-product (cdr tup1) (cdr tup2))))))

(pairwise-product '(1 2 3) '(1 2 3))
(pairwise-product '(5 6) '(7 8))
(pairwise-product '(6) '(6))
(pairwise-product '() '())


;;;; Exercise 3.3: dot-product
(define dot-product
  (lambda (tup1 tup2)
    (sum (pairwise-product tup1 tup2))))

(define dot-product
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) 0)
     (else (+ (* (car tup1) (car tup2))
	      (dot-product (cdr tup1) (cdr tup2)))))))


(dot-product '(3 4 -1) '(1 -2 -3))
(dot-product '(0.003 0.035) '(8 2))
(dot-product '(5.3e4) '(2.0e-3))
(dot-product '() '())

;;;; Exercise 3.4: mult-by-n
(define mult-by-n
  (lambda (num ntpl)
    (if (null? ntpl)
	'()
	(cons (* (car ntpl) num)
	      (mult-by-n num (cdr ntpl))))))

(mult-by-n 3 '(1 2 3 4 5))
(mult-by-n 0 '(1 3 5 7 9 11))
(mult-by-n -7 '())

;;;; Exercise 3.5: index
(define index
  (lambda (a ls)
    (cond
     ((null? ls) -1)
     ((equal? (car ls) a) 0)
     (else (if (eq? (index a (cdr ls)) -1)
	       -1
	       (add1 (index a (cdr ls))))))))

(index 3 '(1 2 3 4 5 6))
(index 'so '(do re me fa so la ti do))
(index 'a '(b c d e))
(index 'cat '())

;;;; Exercise 3.6: make-list

(define make-list
  (lambda (num a)
    (if (zero? num)
	'()
	(cons a (make-list (sub1 num) a)))))


(define all-same?
  (lambda (ls)
    (cond
     ((null? ls) #t)
     ((null? (cdr ls)) #t)
     ((equal? (car ls) (cadr ls))
      (all-same? (cdr ls)))
     (else #f))))

(make-list 5 'no)
(make-list 1 'maybe)
(make-list 0 'yes)
(length (make-list 7 'any))
(all-same? (make-list 100 'any))

;;;; Exercise 3.7: count-background
(define count-background
  (lambda (a ls)
    (cond
     ((null? ls) 0)
     ((equal? (car ls) a) (count-background a (cdr ls)))
     (else (add1 (count-background a (cdr ls)))))))

(count-background 'blue '(red white blue yellow blue red))
(count-background 'red '(white blue green))
(count-background 'white '())


;;;; Exercise 3.8: list-front
(define list-front
  (lambda (ls num)
    (cond
     ((<= (length ls) num)
      (error "Error: length of" ls "is less than" num))
     ((zero? num) '())
     (else (cons (car ls)
		 (list-front (cdr ls) (sub1 num)))))))

(list-front '(a b c d e f g) 4)
(list-front '(a b c) 4)
(list-front '(a b c d e f g) 0)
(list-front '() 3)

;;;; Exercise 3.9: wrapa

(define wrapa
  (lambda (a num)
    (cond
     ((zero? num) a)
     (else (cons (wrapa a (sub1 num)) '())))))

(wrapa 'gift 1)
(wrapa 'sandwich 2)
(wrapa 'prisoner 5)
(wrapa 'moon 0)

;;;; Exercise 3.10: multiple?

(define multiple?
  (lambda (m n)
    (cond
     ((and (zero? m) (zero? n)) #t)
     ((zero? n) #f)
     ((zero? (remainder m n)) #t)
     (else #f))))

(multiple? 7 2)
(multiple? 9 3)
(multiple? 5 0)
(multiple? 0 20)
(multiple? 17 1)
(multiple? 0 0)

;;;; Exercise 3.11: sum-of-odds

;; S = 1 + 3 + 5 + ... + (2n -1)
;; S = (2n-1) + (2n-3) + ... + 3 + 1
;; 2S = 2n + 2n + ... + 2n
;; 2S = n(2n)
;; S = n^2

;; (sum-of-odds n) = (2n-1) + (sum-of-odds (n-1))
;; (sum-of-odds n) = (2n-1) + (2(n-1)-1)         +(sum-of-odds (n-2))
;; (sum-of-odds n) = (2n-1) + (2n-3)             +(sum-of-odds (n-2))
;; (sum-of-odds n) = (2n-1) + (2n-3)             + (2(n-2)-1)         + (sum-of-odds (n-3))
;; (sum-of-odds n) = (2n-1) + (2n-3)             + (2n-5)             + (sum-of-odds (n-3))


(define sum-of-odds
  (lambda (n)
    (cond
     ((zero? n) 0)
     (else (+ (sub1 (* n 2))
	      (sum-of-odds (sub1 n)))))))


(sum-of-odds 0)
(sum-of-odds 1)
(sum-of-odds 2)
(sum-of-odds 4)
(sum-of-odds 5)
(sum-of-odds 6)
(sum-of-odds 7)
(sum-of-odds 8)
(sum-of-odds 9)
(sum-of-odds 10)


;;;; Exercise 3.12: n-tuple->integer
(define n-tuple->integer
  (lambda (tup)
    (cond
     ((null? tup)
      (error "Error: bad argument" tup "to n-tuple->integer"))
     ((null? (cdr tup)) (car tup))
     (else (+ (* (car tup) (expt 10 (sub1(length tup))))
	      (n-tuple->integer (cdr tup)))))))

(n-tuple->integer '(5))
(n-tuple->integer '(1 2))
(n-tuple->integer '(3 1 4 6))
(n-tuple->integer '(0))
(n-tuple->integer '())
(+ (n-tuple->integer '(1 2 3)) (n-tuple->integer '(3 2 1)))

;;;; Exercise 3.13

(define list-ref
  (lambda (ls n)
    (cond
     ((<= (length ls) n)
      (error "list-ref: Index" n "out of range for list" ls))
     ((zero? n) (car ls))
     (else (list-ref (cdr ls) (sub1 n))))))

;;(list-ref ls 4)
;; will take 4 cdr to (car ls) 4th element + 4 * (1000 cdr because of length)  = 4004 cdring
;;---------
(define list-ref
  (lambda (ls n)
    (cond
     ((<= (length ls) n)
      (error "list-ref: Index" n "out of range for list" ls))
     (else (list-ref-helper ls n)))))

(define list-ref-helper
  (lambda (ls n)
    (if (zero? n)
	  (car ls)
	  (list-ref-helper (cdr ls) (sub1 n)))))

;;(list-ref ls 4)
;; will take 4 cdr to (car ls) 4th element +  (1000 cdr because of length)  = 1004 cdring
;;-----------
(define list-ref
  (lambda (ls n)
    (cond
     ((null? ls)
      (error "list-ref: Index" n "out of range for list" ls))
     ((zero? n) (car ls))
     (else (list-ref (cdr ls) (sub1 n))))))

;;(list-ref ls 4)
;; will take only 4 cdr to (car ls) 4th element = 4 cdring

