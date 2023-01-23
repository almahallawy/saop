;; -*- mode: scheme; geiser-scheme-implementation: guile -*-

;; Chapter 5: Locally Defined Procedures

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


(let ((fact (lambda (n)
	      (if (zero? n)
		  1
		  (* n (fact (1- n)))))))
  (fact 4))

;;==>Error
;; ice-9/boot-9.scm:1669:16: In procedure raise-exception:
;; Unbound variable: fact

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



;;Ex 5.1

(let ((a 5)) ;; Env1
  (let ((fun (lambda (x) (max x a)))) ;; Env2, a = 5
    (let ((a 10);;Evn3
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



;; Ex 5.2
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


(letrec
    ((loop
      (lambda (n)
	(if (zero? n)
	    0
	    (+ (remainder n 10)
	       (loop (quotient n 10)))))))
  (loop 1234))

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

