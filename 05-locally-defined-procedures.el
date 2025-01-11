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
