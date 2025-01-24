;; -*- mode: Lisp;-*-

;; Chapter 5: Locally Defined Procedures

(lisp-interaction-mode)

;;We need to enable Lexical binding in elisp. The default binding is Dynamic
;;check the following: 
;;https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
(setq lexical-binding t) ;; important to include this

((lambda (x)
   ((lambda (y)
      (- x y))
    15))
 20)

(let ((a 2) (b 3))
  (+ a b))

(let ((a (function +)) (b 3))
  (funcall a 2 b))

(let ((add2 (lambda (x) (+ x 2)))
      (b (* 3 (/ 2.0 12))))
  (/ b (funcall add2 b)))

(setq a 5)

(1+ 5)

(let ((a 3))
  (1+ a))

(1+ a)


(let ((a 5))
  (progn
    (message "%d" (1+ a))
    (let ((a 3))
      (message "%d" (1+ a)))
    (1+ a)))

(let ((a 2) (b 3))
  (+ a b))
;; is equivleant to
((lambda (a b) (+ a b)) 2 3)


(setq a 10)
(setq b 2)

(let ((a (+ a 5)))
  (* a b))

(let ((a 10)( b 2))
  (let ((a (+ a  5)))
    (* a b)))

(setq addb
      (let ((b 100))
	(lambda (x)
	  (+ x b))))

(let ((b 10))
  (funcall addb 25))

(let ((b 2))
  (let ((add2 (lambda (x) (+ x b)))
	(b 0.5))
    (/ b (funcall add2 b))))


(listp '())
(consp '())

(defun remove-leftmost (item ls)
  (cond
   ((null ls) '())
   ((equal (car ls) item) (cdr ls))
   ((consp (car ls))
    (let ((rem-list (remove-leftmost item (car ls))))
      (cons rem-list (cond
		      ((equal (car ls) rem-list)
		       (remove-leftmost item (cdr ls)))
		      (t (cdr ls))))))
   (t (cons (car ls)
	    (remove-leftmost item (cdr ls))))))

(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))


;; the following code will result in error:
;; "Debugger entered--Lisp error: (void-function fact)"
;; This message refers to the fact occuring in the lambda expression,
;;which is not bound outside of the let expression
(let ((fact (lambda (n)
	      (if (zerop n)
		  1
		  (* n (fact (1- n)))))))
  (fact 4))

(letrec ((fact (lambda (n)
	      (if (zerop n)
		  1
		  (* n (funcall fact (1- n)))))))
  (funcall fact 4))

;;Mutual recursion
(letrec ((even? (lambda (x)
		  (or (zerop x) (funcall odd? (1- x)))))
	 (odd?  (lambda (x)
		  (and (not (zerop x)) (funcall even? (1- x))))))
  (funcall odd? 17))



(defun fact (n)
  (letrec ((fact-it 
            (lambda (k acc)
              (if (zerop k)
                  acc
                (funcall fact-it (1- k) (* k acc))))))
    (funcall fact-it n 1)))  

(fact 4)

(defun swapper (x y ls)
  (letrec
      ((swap
        (lambda (ls*)
          (cond 
           ((null ls*) '())
           ((equal (car ls*) x) (cons y (funcall swap (cdr ls*))))
           ((equal (car ls*) y) (cons x (funcall swap (cdr ls*))))
           (t (cons (car ls*) (funcall swap (cdr ls*))))))))
    (funcall swap ls)))

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
      (funcall fun 1))))
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

;; Note the differnce between Clojure and Scheme/elsip in let evaluatoin order
;; in Clojure, the bindings in a let form are evaluated in the order they are written, and each binding can depend on the previous ones. This means you can use the value of a previously defined binding in the definition of a subsequent binding.
;;This code will fail in scheme/elisp
(let ((a 1)
      (b a))
  (+ a b))


;; Ex 5.2
(letrec
    ((loop
      (lambda (n k)
	(cond
	 ((zerop k) n)
	 ((< n k) (funcall loop k n))
	 (t (funcall loop k (mod n k)))))))
  (funcall loop 9 12))

;; (loop 9 12)
;; (loop 12 9) ; (< 9 12)
;; (loop 9 (remainder 12 9)) -> (loop 9 3)
;; (loop 3 (remainder 9 3)) -> (loop 3 0)
;; 3 (zero? k=0)


;;b
(letrec
    ((loop
      (lambda (n)
	(if (zerop n)
	    0
	    (+ (mod n 10)
	       (funcall loop (truncate n 10)))))))
  (funcall loop 1234))
;;calculates the sum of the digits of a given number


;;Ex5.3

((lambda (a)
   ((lambda (fun)
      ((lambda (a x)
	 (funcall fun 1)) 10 20))
    (lambda (x) (max x a)))) 5)


((lambda (a b)
   ((lambda (b c)
      ((lambda (b)
	 (cons a (cons b (cons c '())))) 5))
    3 (+ a b)))
 1 2)


;;Ex5.4

;; Do NOT create append functiln as it will provide the built-in elisp
;; Append is a core function, redefining it will break emacs
(defun even? (int)
  (if (zerop int)
      t
    (odd? (1- int))))

(defun odd? (int)
  (if (zerop int)
      nil
    (even? (1- int))))

(letrec ((mystery
	  (lambda (tuple odds evens)
	    (if (null tuple)
		(append odds evens)
		(let ((next-int (car tuple)))
		  (if (odd? next-int)
		      (funcall mystery (cdr tuple)
			       (cons next-int odds) evens)
		      (funcall mystery (cdr tuple)
			       odds (cons next-int evens))))))))
  (funcall mystery '(3 16 4 7 9 12 24) '() '()))

;;Create a list where odds comes first then evens
;;=> (9 7 3 24 12 4 16)


;;Ex 5.5
(defun mystery (n)
  (letrec
      ((mystery-helper
	(lambda (n s)
	  (cond
	   ((zerop n) (list s))
	   (t
	    (append
	     (funcall mystery-helper (1- n) (cons 0 s))
	     (funcall mystery-helper (1- n) (cons 1 s))))))))
    (funcall mystery-helper n '())))

(mystery 4);;All binary numbers of 4 bits = 2^4 = 16
;; => ((0 0 0 0) (1 0 0 0) (0 1 0 0) (1 1 0 0) (0 0 1 0) (1 0 1 0) (0 1 1 0) (1 1 1 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (1 0 1 1) (0 1 1 1) (1 1 1 1))

(mystery 3);;All binary numbers of 3 bits = 2^3 = 8
;; => ((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1))

;;(mystery n) All binary numbers of n bits = 2^n


;;Ex 5.6
(defun insert-left-all (new old ls)
  (letrec
      ((insert-la
	(lambda (ls)
	  (cond
	   ((null ls) '())
	   ((eq (car ls) old)
	    (cons new (cons old (funcall insert-la (cdr ls)))))
	   ((consp (car ls))
	    (cons (funcall insert-la (car ls))
		  (funcall insert-la (cdr ls))))
	   (t (cons (car ls)
		    (funcall insert-la (cdr ls))))))))
    (funcall insert-la ls)))

(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())


;;Ex5.7
(defun fib (n)
  (letrec ((fib-it
	    (lambda (n acc1 acc2)
	      (if (eq n 1)
		  acc2
		(funcall fib-it (1- n) acc2 (+ acc1 acc2))))))
    (funcall fib-it n 0 1)))

(fib 3)


;;Ex5.8
(defun list-ref (ls n)
  (letrec ((list-ref-helper
	    (lambda (ls n)
	      (if (zerop n)
		  (car ls)
		(funcall list-ref-helper (cdr ls) (1- n))))))
    (if (<= (length ls) n)
	 (error  "Index %d out of range for list %s" n ls)
      (funcall list-ref-helper ls n))))

(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)q
(list-ref '() 0)

;;5.3 Symbolic manipulation of Polynomials

; - Program 5.14, pg. 151 -
;; The five basic defintion (version I)

(defun sub1 (n)
  (1- n))

(defun list-of-zeros (n)
  (cond
   ((zerop n) '())
   (t (cons 0 (list-of-zeros (sub1 n))))))

(list-of-zero 5)

(setq the-zero-poly '(0))

(defun degree (poly)
  (sub1 (length poly)))

(defun leading-coef (poly)
  (car poly))

(defun rest-of-poly (poly)
  (cond
   ((zerop (degree poly)) the-zero-poly)
   ((zerop (leading-coef (cdr poly)))
    (rest-of-poly (cdr poly)))
   (t (cdr poly))))

(defun poly-cons (deg coef poly)
  (let ((deg-p (degree poly)))
    (cond
     ((and (zerop deg) (equal poly the-zero-poly)) (list coef))
     ((< deg-p deg)
      (if (zerop coef)
          poly
        (cons coef 
              (append (list-of-zeros (sub1 (- deg deg-p))) 
                      poly))))
     (t (error "poly-cons: Degree too high in %s" poly)))))

;; - End Program -


; - Program 5.15, pg. 153 -
;; The five basic defintion (version II)

(setq the-zero-poly '((0 0)))

(defun degree (poly)
  (caar poly))

(defun leading-coef (poly)
  (cadar poly))

(defun rest-of-poly (poly)
  (if (null (cdr poly))
      the-zero-poly
    (cdr poly)))

(defun poly-cons (deg coef poly)
  (let ((deg-p (degree poly)))
    (cond 
     ((and (zerop deg) (equal poly the-zero-poly))
      (list (list deg coef)))
     ((< deg-p deg)
      (if (zerop coef) 
          poly
        (cons (list deg coef) poly)))
     (t (error "poly-cons: degree too high in %s" poly)))))

;; - End Program -

; - Program 5.6, pg. 144 -

(defun zero-poly? (poly)
  (and (zerop (degree poly)) (zerop (leading-coef poly))))

; - End Program -

; - Program 5.7, pg. 145 -

(defun make-term (deg coef)
  (poly-cons deg coef the-zero-poly)) 

; - End Program -

; - Program 5.8, pg. 145 -

(defun leading-term (poly)
  (make-term (degree poly) (leading-coef poly)))

; - End Program -

; - Program 5.9, pg. 146 -

(defun p+ (poly1 poly2)
  (cond
   ((zero-poly? poly1) poly2)
   ((zero-poly? poly2) poly1)
   (t (let ((n1 (degree poly1))
               (n2 (degree poly2))
               (a1 (leading-coef poly1))
               (a2 (leading-coef poly2))
               (rest1 (rest-of-poly poly1))
               (rest2 (rest-of-poly poly2)))
           (cond 
            ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
            ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
            (t
             (poly-cons n1 (+ a1 a2) (p+ rest1 rest2)))))))) 

; - End Program -

; - Program 5.10, pg. 148 -

(setq p* 
  (letrec 
    ((t* (lambda (trm poly)
           (if (zero-poly? poly)
               the-zero-poly
               (poly-cons 
                 (+ (degree trm) (degree poly))
                 (* (leading-coef trm) (leading-coef poly))
                 (funcall t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
        ((p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                          the-zero-poly
                          (p+ (funcall t* (leading-term p1) poly2)
                              (funcall p*-helper (rest-of-poly p1)))))))
        (funcall p*-helper poly1)))))

;;Organize the function little bit different than the book
;;move p*-helper to letrec of t*

(defun p* (poly1 poly2) 
  (letrec 
      ((t* (lambda (trm poly)
             (if (zero-poly? poly)
		 the-zero-poly
	       (poly-cons 
                (+ (degree trm) (degree poly))
                (* (leading-coef trm) (leading-coef poly))
                (funcall t* trm (rest-of-poly poly))))))
       (p*-helper (lambda (p1)
                    (if (zero-poly? p1)
                        the-zero-poly
                      (p+ (funcall t* (leading-term p1) poly2)
                          (funcall p*-helper (rest-of-poly p1)))))))
    (funcall p*-helper poly1)))

;; - End Program -
;; - Program 5.11, pg. 148 -

(defun negative-poly (poly) 
  (let ((poly-negative-one (make-term 0 -1)))
    (p* poly-negative-one poly)))

; - End Program -

; - Program 5.12, pg. 148 -

(defun p- (poly1 poly2)
  (p+ poly1 (negative-poly poly2)))  

; - End Program -

; - Program 5.13, pg. 150 -

(defun poly-value (poly num)
  (letrec 
      ((pvalue (lambda (p)
                 (let ((n (degree p)))
                   (if (zerop n) 
                       (leading-coef p)
                     (let ((rest (rest-of-poly p)))
                       (if (< (degree rest) (sub1 n))
                           (funcall pvalue (poly-cons
                                    (sub1 n)
                                    (* num (leading-coef p))
                                    rest))
                         (funcall pvalue (poly-cons 
                                  (sub1 n)
                                  (+ (* num (leading-coef p))
                                     (leading-coef rest))
                                  (rest-of-poly rest))))))))))
    (funcall pvalue poly)))


;;
;;3x^4 + 5x^2 + 12
(setq p1
  (poly-cons 4 3
	     (poly-cons 2 5
			(poly-cons 0 12 the-zero-poly))))

;;7x^5 + 6x4 - x^2 + 11x - 15
(setq p2
  (poly-cons 5 7
	     (poly-cons 4 6
			(poly-cons 2 -1
				   (poly-cons 1 11
					      (poly-cons 0 -15 the-zero-poly))))))

;; p1+p2 = 7x^5 + 9x^4 + 4x^2 + 11x-3
(poly-value (p+ p1 p2) 1) ;;7+9+4+11-3=28


;;Ex5.9
;;p1(x) = 5x^4 - 7x^3 +        2x - 4
;;p2(x) =         x^3 + 6x^2 - 3x

(setq p1
  (poly-cons 4 5
	     (poly-cons 3 -7
			(poly-cons 1 2
				   (poly-cons 0 -4 the-zero-poly)))))

(setq p2
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

;;Ex5.10
(defun p+ (poly1 poly2)
  (cond
   ((zero-poly? poly1) poly2)
   ((zero-poly? poly2) poly1)
   (t (let ((n1 (degree poly1))
               (n2 (degree poly2)))
           (cond
            ((> n1 n2)
	     (let ((a1 (leading-coef poly1))
		   (rest1 (rest-of-poly poly1)))
	       (poly-cons n1 a1 (p+ rest1 poly2))))

            ((< n1 n2)
	     (let ((a2 (leading-coef poly2))
		   (rest2 (rest-of-poly poly2)))
	       (poly-cons n2 a2 (p+ poly1 rest2))))

            (t
	     (let ((a1 (leading-coef poly1))
                   (a2 (leading-coef poly2))
                   (rest1 (rest-of-poly poly1))
                   (rest2 (rest-of-poly poly2)))
               (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))) 


;;Ex5.11


(defun poly-quotient (p1 p2)
  (let ((a2 (leading-coef p2))
	(n2 (degree p2)))
    (letrec
	((pq-helper
	  (lambda (poly)
	    (cond
	     ((zero-poly? poly) the-zero-poly)
	     ((< (degree poly) n2) the-zero-poly)
	     (t (let ((term (make-term
				(- (degree poly) n2)
				(/ (leading-coef poly) a2))))
		     (p+ term
			 (funcall pq-helper (p- poly
					(p* p2 term))))))))))
      (funcall pq-helper p1))))


(defun poly-remainder (p1 p2)
  (let ((a2 (leading-coef p2))
	(n2 (degree p2)))
    (letrec
	((pr-helper
	  (lambda (poly)
	    (cond
	     ((zero-poly? poly) the-zero-poly)
	     ((< (degree poly) n2) poly)
	     (t (let ((term (make-term
				(- (degree poly) n2)
				(/ (leading-coef poly) a2))))
		     (funcall pr-helper (p- poly
				    (p* p2 term)))))))))
      (funcall pr-helper p1))))

;;p1 = x^3 - 2x^2 - 4
(setq p1
  (poly-cons 3 1
	     (poly-cons 2 -2
			(poly-cons 0 -4 the-zero-poly))))

(poly-value p1 1)

;;p2 = x - 3
(setq p2
  (poly-cons 1 1
	     (poly-cons 0 -3 the-zero-poly)))

(poly-value p2 1)

;; p1 = p2 * q + r
;;p1 / p2 = (x -3)(x^2 + x + 3) + 5


(poly-value (poly-quotient p1 p2) 1) ;; 1+1+3=5
(poly-value (poly-remainder p1 p2) 1)

;;------------------------
;;p1 = x^2 - 3x - 10
(setq p1
  (poly-cons 2 1
	     (poly-cons 1 -3
			(poly-cons 0 -10 the-zero-poly))))

(poly-value p1 1)

;;p2 = x + 2
(setq p2
  (poly-cons 1 1
	     (poly-cons 0 2 the-zero-poly)))

(poly-value p2 1)

;; p1 = p2 * q + r
;;p1 / p2 = (x + 2)(x - 5) + 0

(poly-value (poly-quotient p1 p2) 1)
(poly-value (poly-remainder p1 p2) 1)
;;------------------
;;p1 =2x^2 - 5x - 1
(setq p1
  (poly-cons 2 2
	     (poly-cons 1 -5
			(poly-cons 0 -1 the-zero-poly))))

(poly-value p1 1)


;;p2 = x - 3
(setq p2
  (poly-cons 1 1
	     (poly-cons 0 -3 the-zero-poly)))


(poly-value p2 1)

;; p1 = p2 * q + r
;;p1 / p2 = (x - 3)(2x +1) + 2

(poly-value (poly-quotient p1 p2) 1)
(poly-value (poly-remainder p1 p2) 1)

;;---------------------------------
;;Ex5.12
;;If polynomial presentation in the order of increasing dgre


;;degree: Will be same implementation for both increasing and decreasing
;;leading-coef: will be needed for quotient and remainder and will need to reverse the list or traverse it to get last element. 
;; p+,p*,p- will need to be adjusted little bit, but it will be same compelxity of decreasing
;;however quotiesn/remainder will get more complex and division relies on the highest degree term.

;;Ex5.13
;;(cons deg coef) instead of (list deg coef)

(cdr (list 1 2)) ;;(2)
(cdr (cons 1 2)) ;; 2


(car (list 1 2)) ;;1
(car (cons 1 2)) ;;1

(cdar (list (cons 1 2)))

(defun poly-cons (deg coef poly)
  (let ((deg-p (degree poly)))
    (cond 
     ((and (zerop deg) (equal poly the-zero-poly))
      (list (cons deg coef)))
     ((< deg-p deg)
      (if (zerop coef) 
	  poly
	(cons (cons deg coef) poly)))
     (t (error "poly-cons: degree too high in %s" poly)))))


(defun leading-coef (poly)
  (cdar poly))

;;
;;3x^4 + 5x^2 + 12
(setq p1
  (poly-cons 4 3
	     (poly-cons 2 5
			(poly-cons 0 12 the-zero-poly))))

(degree p1)
(leading-coef p1)

;;7x^5 + 6x4 - x^2 + 11x - 15
(setq p2
  (poly-cons 5 7
	     (poly-cons 4 6
			(poly-cons 2 -1
				   (poly-cons 1 11
					      (poly-cons 0 -15 the-zero-poly))))))

(degree p2)
(leading-coef p2)

;;---
;;5.14

(setq p* 
  (letrec 
    ((t* (lambda (trm poly)
	   (let ((deg (degree trm)) 
		 (lc (leading-coef trm)))
	     (letrec
		 ((t*-helper
		   (lambda (poly)
		     (if (zero-poly? poly)
			 the-zero-poly
			 (poly-cons 
			  (+ deg (degree poly))
			  (* lc (leading-coef poly))
			  (funcall t*-helper (rest-of-poly poly)))))))
	       (funcall t*-helper poly))))))
    (lambda (poly1 poly2)
      (letrec
        ((p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                          the-zero-poly
                          (p+ (funcall t* (leading-term p1) poly2)
                              (funcall p*-helper (rest-of-poly p1)))))))
        (funcall p*-helper poly1)))))

;;Organize and looks niceer with elisp
(defun p* (poly1 poly2)
  (letrec 
    ((t* (lambda (trm poly)
	   (let ((deg (degree trm)) 
		 (lc (leading-coef trm)))
	     (letrec
		 ((t*-helper
		   (lambda (poly)
		     (if (zero-poly? poly)
			 the-zero-poly
			 (poly-cons 
			  (+ deg (degree poly))
			  (* lc (leading-coef poly))
			  (funcall t*-helper (rest-of-poly poly)))))))
	       (funcall t*-helper poly)))))
     (p*-helper (lambda (p1)
                    (if (zero-poly? p1)
                        the-zero-poly
                      (p+ (funcall t* (leading-term p1) poly2)
                          (funcall p*-helper (rest-of-poly p1)))))))
    (funcall p*-helper poly1)))




;;By defining deg and lc in the let above, we are able to avoid evaluating
;; (degree trm) and (leading-coef trm).
;;Also by defining t*-helper in the let part, we don't need to pass trm anymore


;;---------------
;;Ex5.15

;;Recursive
(defun append-to-list-of-zeros (n x)
  (letrec
      ((append-helper
	(lambda (n)
	  (cond
	   ((zerop n) x)
	   (t (cons 0 (funcall append-helper (sub1 n))))))))
    (funcall append-helper n)))

(append-to-list-of-zeros 1 '(a b))

(defun poly-cons (deg coef poly)
  (let ((deg-p (degree poly)))
    (cond
     ((and (zerop deg) (equal poly the-zero-poly)) (list coef))
     ((< deg-p deg)
      (if (zerop coef)
	  poly
	(cons coef (append-to-list-of-zeros
		    (sub1 (- deg  deg-p)) poly))))
     (else (error "poly-cons: Degree too high in %s" poly)))))

;;Iterative list-of-zeros
(defun list-of-zeros (n)
  (letrec ((list-of-zeros-it
	    (lambda (n acc)
	      (if (zerop n)
		  acc
		(funcall list-of-zeros-it (sub1 n) (cons 0 acc))))))
    (funcall list-of-zeros-it n '())))

(list-of-zeros 5)

;;Iterative
(defun append-to-list-of-zeros  (n x)
  (letrec
      ((append-helper
	(lambda (n acc)
	  (if (zerop n)
	      acc
	    (funcall append-helper (sub1 n) (cons 0 acc))))))
    (funcall append-helper n x)))

(append-to-list-of-zeros 5 '())
