;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 5: Locally Defined Procedures

((lambda (x)
   ((lambda (y)
      (- x y))
    15))
 20)

(let ((a +) (b 3))
  (a 2 b))

(let ((add2 (lambda (x) (+ x 2)))
      (b (* 3 (/ 2 12))))
  (/ b (add2 b)))

(let ((a 2) (b 3))
  (+ a b))
;; is equivelant to
((lambda (a b) (+ a b)) 2 3)


(define addb
  (let ((b 100))
    (lambda (x)
      (+ x b))))

(let ((b 10))
  (addb 25))


(let ((b 2))
  (let ((add2 (lambda (x) (+ x b)))
	(b 0.5))
    (/ b (add2 b))))



(define remove-leftmost
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     ((pair? (car ls))
      (let ((rem-list (remove-leftmost item (car ls))))
	(cons rem-list
	      (cond
	       ((equal? (car ls) rem-list)
		(remove-leftmost item (cdr ls)))
	       (else (cdr ls))))))
     (else (cons (car ls)
		 (remove-leftmost item (cdr ls)))))))


(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))

;; the following code will result in error:
;; "Unbound variable: fact"
;; This message refers to the fact occuring in the lambda expression,
;;which is not bound outside of the let expression
(let ((fact (lambda (n)
	      (if (zero? n)
		  1
		  (* n (fact (1- n)))))))
  (fact 4))


(let ((fact (lambda (n)
	      (if (zero? n)
		  1
		  (* n (fact (1- n)))))))
  (fact 0))
;;==> 1


(letrec ((fact (lambda (n)
	      (if (zero? n)
		  1
		  (* n (fact (1- n)))))))
  (fact 4))


;;Mutual recursion
(letrec ((even? (lambda (x)
		  (or (zero? x) (odd? (1- x)))))
	 (odd?  (lambda (x)
		  (and (not (zero? x)) (even? (1- x))))))
  (odd? 17))


(define fact
  (lambda (n)
    (letrec ((fact-it 
               (lambda (k acc)
                 (if (zero? k)
                     acc
                     (fact-it (1- k) (* k acc))))))
      (fact-it n 1))))  

(fact 4)


(define swapper
  (lambda (x y ls)
    (letrec
      ((swap
         (lambda (ls*)
           (cond 
             ((null? ls*) '())
             ((equal? (car ls*) x) (cons y (swap (cdr ls*))))
             ((equal? (car ls*) y) (cons x (swap (cdr ls*))))
             (else (cons (car ls*) (swap (cdr ls*))))))))
      (swap ls))))

(swapper 'cat 'dog '(my cat eats dog food))
(swapper 'john 'mary '(john loves mary))
(swapper 'a 'b  '(c (a b) d))
(swapper 'a 'b '())
(swapper 'b 'd '(a b c d b))


;;Ex 5.1

(let ((a 5))			      ;;Env1
  (let ((fun (lambda (x) (max x a)))) ;Env2, a bound to 5 in Env1, x bound to 1 from fn param passing
    (let ((a 10)		      ;;Evn3
	  (x 20))
      (fun 1))))
;;=> 5

;; Env1: a = 5

;; Env2: fun -> x , (max x a=5) , Env1

;; Env3: a = 10, x = 20



(let ((a 1) (b 2)) ;Env1
  (let ((b 3) (c (+ a b))) ;Env2
    (let ((b 5)) ;Env3
      (cons a (cons b (cons c '()))))))
;;=> (1 5 3)
;; Env1: a=1 b=2
;; Env2: b=3, (+ a=1 b=2) => c=3
;; Env3: b=5 (cons a=1 (cons b=5 (cons c=3 '())))

;; Note the differnce between Clojure in Scheme in let evaluatoin order
;; in Clojure, the bindings in a let form are evaluated in the order they are written, and each binding can depend on the previous ones. This means you can use the value of a previously defined binding in the definition of a subsequent binding.
;;This code will fail in scheme
(let ((a 1)
      (b a))
  (+ a b))


;; Ex 5.2
;;b
(letrec
    ((loop
      (lambda (n k)
	(cond
	 ((zero? k) n)
	 ((< n k) (loop k n))
	 (else (loop k (remainder n k)))))))
  (loop 9 12))

;; (loop 9 12)
;; (loop 12 9) ; (< 9 12)
;; (loop 9 (remainder 12 9)) -> (loop 9 3)
;; (loop 3 (remainder 9 3)) -> (loop 3 0)
;; 3 (zero? k=0)

;;b
(letrec
    ((loop
      (lambda (n)
	(if (zero? n)
	    0
	    (+ (remainder n 10)
	       (loop (quotient n 10)))))))
  (loop 1234))
;;calculates the sum of the digits of a given number
(remainder 1234 10)
(quotient 1234 10)

;; Ex 5.3

;;start converting from outer let
(let ((a 5))
  (let ((fun (lambda (x) (max x a))))
    (let ((a 10)
	  (x 20))
      (fun 1))))

;; (let ((a 5)) boady)
;; ==
;; ((lambda (a)
;;    body) 5)

((lambda (a)
   (let ((fun (lambda (x) (max x a))))
    (let ((a 10)
	  (x 20))
      (fun 1)))) 5)


;; (let ((fun (lambda (x) (max x a))))
;;   body)
;; ==
;; ((lambda (fun) boday)
;;  (lambda (x) (max x a)))

((lambda (a)
   ((lambda (fun)
      (let ((a 10)
	    (x 20))
	(fun 1)))
    (lambda (x) (max x a)))) 5)


;; (let ((a 10)
;;       (x 20))
;;   (fun 1))
;; ==
;; ((lambda (a x)
;;    (fun 1)) 10 20)


((lambda (a)
   ((lambda (fun)
      ((lambda (a x)
	 (fun 1)) 10 20))
    (lambda (x) (max x a)))) 5)

;;-----------------------------

(let ((a 1) (b 2))
  (let ((b 3) (c (+ a b)))
    (let ((b 5))
      (cons a (cons b (cons c '()))))))

;; start converting from inner let
;; (let ((b 5))
;;   (cons a (cons b (cons c '()))))
;; ==
 ;; ((lambda (b)
;;    (cons a (cons b (cons c '())))) 5)

(let ((a 1) (b 2))
  (let ((b 3) (c (+ a b)))
    ((lambda (b)
       (cons a (cons b (cons c '())))) 5)))

;; (let ((b 3) (c (+ a b)))
;;     ((lambda (b)
;;        (cons a (cons b (cons c '())))) 5))
;; ==
;; ((lambda (b c)
;;    ((lambda (b)
;;        (cons a (cons b (cons c '())))) 5))
;;  3 (+ a b))


(let ((a 1) (b 2))
  ((lambda (b c)
     ((lambda (b)
	(cons a (cons b (cons c '())))) 5))
   3 (+ a b)))
;;==
((lambda (a b)
   ((lambda (b c)
      ((lambda (b)
	 (cons a (cons b (cons c '())))) 5))
    3 (+ a b)))
 1 2)


;;==========================
;; Ex 5.4

(letrec ((mystery
	  (lambda (tuple odds evens)
	    (if (null? tuple)
		(append odds evens)
		(let ((next-int (car tuple)))
		  (if (odd? next-int)
		      (mystery (cdr tuple)
			       (cons next-int odds) evens)
		      (mystery (cdr tuple)
			       odds (cons next-int evens))))))))
  (mystery '(3 16 4 7 9 12 24) '() '()))

;;Create a list where odds comes first then evens
;;=> (9 7 3 24 12 4 16)

(define append
  (lambda (ls1 ls2)
    (if (null? ls1)
	ls2
	(cons (car ls1) (append (cdr ls1) ls2)))))

(define even?
  (lambda (int)
    (if (zero? int)
	#t
	(odd? (1- int)))))

(define odd?
  (lambda (int)
    (if (zero? int)
	#f)
    (even? (1- int))))



;; Ex5.5
(define mystery
  (lambda (n)
    (letrec
	((mystery-helper
	  (lambda (n s)
	    (cond
	     ((zero? n) (list s))
	     (else
	      (append
	       (mystery-helper (1- n) (cons 0 s))
	       (mystery-helper (1- n) (cons 1 s))))))))
      (mystery-helper n '()))))

(mystery 4);;All binary numbers of 4 bits = 2^4 = 16
;; => ((0 0 0 0) (1 0 0 0) (0 1 0 0) (1 1 0 0) (0 0 1 0) (1 0 1 0) (0 1 1 0) (1 1 1 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (1 0 1 1) (0 1 1 1) (1 1 1 1))

(mystery 3);;All binary numbers of 3 bits = 2^3 = 8
;; => ((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1))

;;(mystery n) All binary numbers of n bits = 2^n


;; Ex 5.6: insert-left-all
(define insert-left-all
  (lambda (new old ls)
    (letrec
	((insert-left
	  (lambda (ls)
	    (cond
	     ((null? ls) '())
	     ((pair? (car ls))
	      (cons (insert-left (car ls))
		    (insert-left (cdr ls))))
	     ((equal? (car ls) old)
	      (cons new (cons old
			      (insert-left (cdr ls)))))
	     (else (cons (car ls)
			 (insert-left (cdr ls))))))))
      (insert-left ls))))


(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())



;; Ex 5.7 fib
(define fib
  (lambda (n)
    (letrec
	((fib-it
	  (lambda (n acc1 acc2)
	    (if (= n 1)
		acc2
		(fib-it (1- n) acc2 (+ acc1 acc2))))))
      (if (zero? n)
	  0
	  (fib-it n 0 1)))))

(fib 6)

;; Ex5.8 list-ref
(define list-ref
  (lambda (ls n)
    (letrec
	((list-ref-helper
	  (lambda (ls n)
	    (if (zero? n)
		(car ls)
		(list-ref-helper (cdr ls) (1- n))))))
      (cond
       ((<= (length ls) n)
	(error "list-ref: Index" n "out of range for list" ls))
       (else (list-ref-helper ls n))))))


(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)
(list-ref '() 0)



;;5.3 Symbolic manipulation of Polynomials

; - Program 5.14, pg. 151 -
;; The five basic defintion (version I)

(define sub1
  (lambda (n)
    (1- n)))

(define list-of-zeros
  (lambda (n)
    (cond
     ((zero? n) '())
     (else (cons 0 (list-of-zeros (sub1 n)))))))


(define the-zero-poly '(0))

(define degree 
  (lambda (poly)
    (sub1 (length poly))))

(define leading-coef
  (lambda (poly)
    (car poly)))

(define rest-of-poly
  (lambda (poly)
    (cond
      ((zero? (degree poly)) the-zero-poly)
      ((zero? (leading-coef (cdr poly)))
       (rest-of-poly (cdr poly)))
      (else (cdr poly)))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond
        ((and (zero? deg) (equal? poly the-zero-poly)) (list coef))
        ((< deg-p deg)
         (if (zero? coef)
             poly
             (cons coef 
                (append (list-of-zeros (sub1 (- deg deg-p))) 
                        poly))))
        (else (error "poly-cons: Degree too high in" poly))))))

; - End Program -

; - Program 5.15, pg. 153 -
;; The five basic defintion (version II)

(define the-zero-poly '((0 0)))

(define degree
  (lambda (poly)
    (caar poly)))
    
(define leading-coef
  (lambda (poly)
    (cadar poly)))

(define rest-of-poly 
  (lambda (poly)
    (if (null? (cdr poly))
        the-zero-poly
        (cdr poly))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond 
        ((and (zero? deg) (equal? poly the-zero-poly))
         (list (list deg coef)))
        ((< deg-p deg)
         (if (zero? coef) 
             poly
             (cons (list deg coef) poly)))
        (else (error "poly-cons: degree too high in" poly))))))

; - End Program -



; - Program 5.6, pg. 144 -

(define zero-poly?
  (lambda (poly)
    (and (zero? (degree poly)) (zero? (leading-coef poly)))))

; - End Program -

; - Program 5.7, pg. 145 -

(define make-term
  (lambda (deg coef)
    (poly-cons deg coef the-zero-poly))) 

; - End Program -

; - Program 5.8, pg. 145 -

(define leading-term
  (lambda (poly)
    (make-term (degree poly) (leading-coef poly))))

; - End Program -

; - Program 5.9, pg. 146 -

(define p+
  (lambda (poly1 poly2)
    (cond
      ((zero-poly? poly1) poly2)
      ((zero-poly? poly2) poly1)
      (else (let ((n1 (degree poly1))
                  (n2 (degree poly2))
                  (a1 (leading-coef poly1))
                  (a2 (leading-coef poly2))
                  (rest1 (rest-of-poly poly1))
                  (rest2 (rest-of-poly poly2)))
              (cond 
                ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
                ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
                (else 
                 (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))) 

; - End Program -

; - Program 5.10, pg. 148 -

(define p* 
  (letrec 
    ((t* (lambda (trm poly)
           (if (zero-poly? poly)
               the-zero-poly
               (poly-cons 
                 (+ (degree trm) (degree poly))
                 (* (leading-coef trm) (leading-coef poly))
                 (t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
        ((p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                          the-zero-poly
                          (p+ (t* (leading-term p1) poly2)
                              (p*-helper (rest-of-poly p1)))))))
        (p*-helper poly1)))))

;;Organize the function little bit different than the book
;;move p*-helper to letrec of t*

(define p*
  (lambda (poly1 poly2) 
    (letrec 
	((t* (lambda (trm poly)
               (if (zero-poly? poly)
		   the-zero-poly
		   (poly-cons 
                    (+ (degree trm) (degree poly))
                    (* (leading-coef trm) (leading-coef poly))
                    (t* trm (rest-of-poly poly))))))
	 (p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                          the-zero-poly
                          (p+ (t* (leading-term p1) poly2)
                              (p*-helper (rest-of-poly p1)))))))
      (p*-helper poly1))))


;; - End Program -
;; - Program 5.11, pg. 148 -

(define negative-poly
  (lambda (poly) 
    (let ((poly-negative-one (make-term 0 -1)))
      (p* poly-negative-one poly))))

; - End Program -

; - Program 5.12, pg. 148 -

(define p-
  (lambda (poly1 poly2)
    (p+ poly1 (negative-poly poly2))))  

; - End Program -

; - Program 5.13, pg. 150 -

(define poly-value
  (lambda (poly num)
    (letrec 
      ((pvalue (lambda (p)
                 (let ((n (degree p)))
                   (if (zero? n) 
                       (leading-coef p)
                       (let ((rest (rest-of-poly p)))
                         (if (< (degree rest) (sub1 n))
                             (pvalue (poly-cons
                                       (sub1 n)
                                       (* num (leading-coef p))
                                       rest))
                             (pvalue (poly-cons 
                                       (sub1 n)
                                       (+ (* num (leading-coef p))
                                          (leading-coef rest))
                                       (rest-of-poly rest))))))))))
      (pvalue poly))))

; - End Program 

;;
;;3x^4 + 5x^2 + 12
(define p1
  (poly-cons 4 3
	     (poly-cons 2 5
			(poly-cons 0 12 the-zero-poly))))

;;7x^5 + 6x4 - x^2 + 11x - 15
(define p2
  (poly-cons 5 7
	     (poly-cons 4 6
			(poly-cons 2 -1
				   (poly-cons 1 11
					      (poly-cons 0 -15 the-zero-poly))))))

;; p1+p2 = 7x^5 + 9x^4 + 4x^2 + 11x-3
(poly-value (p+ p1 p2) 1) ;;7+9+4+11-3


;;Ex5.9
;;p1(x) = 5x^4 - 7x^3 +        2x - 4
;;p2(x) =         x^3 + 6x^2 - 3x

(define p1
  (poly-cons 4 5
	     (poly-cons 3 -7
			(poly-cons 1 2
				   (poly-cons 0 -4 the-zero-poly)))))

(define p2
  (poly-cons 3 1
	     (poly-cons 2 6
			(poly-cons 1 -3 the-zero-poly))))

;;p1+p2 = 5x^4 - 6x^3 + 6x^2 - x - 4
(poly-value (p+ p1 p2) 1) ;; 5-6+6-1-4 = 0

;;p1-p2 = 5x^4 - 8x^3 - 6x^2 + 5x - 4
(poly-value (p- p1 p2) 1) ;; 5-8-6+5-4 = -8

;;p1*p2 =  5x^7 + 23x^6 - 57x^5 + 23x^4 + 8x^3 - 30x^2 + 12x
(poly-value (p* p1 p2) 1) ;; = 5 + 23 - 57 + 23 + 8 - 30 + 12 = -16

(poly-value p1 -1) ;; 5 + 7 - 2 -4 = 6
(poly-value p1 2) ;; 5*16 - 7*8 + 2*2 -4 = 24
(poly-value p2 0) ;; 0
(poly-value p2 -2) ;; -8 + 6*4 - 3* -2 = 22
