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



