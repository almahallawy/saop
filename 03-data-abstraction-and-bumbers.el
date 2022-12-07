;; -*- mode: Lisp;-*-

;; Chapter 3 - Data Abstractions and Numbers

(integerp 5)
(integerp 0.1)

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(sub1 4)

(defun harmonic-sum (n)
  (cond
   ((zerop n) 0)
   (t (+ (/ 1 n) (harmonic-sum (sub1 n))))))

(defun list-of-zeros (n)
  (cond
   ((zerop n) '())
   (t (cons 0 (list-of-zeros (sub1 n))))))

(list-of-zeros 5)


(defun =length= (ls)
  (if (null ls)
      0
    (add1 (=length= (cdr ls)))))

(=length= '(1 2 3 4 5))
(length '(1 2 3 4 5))
(=length= '())
(length '())

(error "dfdf %d" 4)

(defun list-ref (ls n)
  (cond
   ((<= (length ls) n)
    (error  "Index %d out of range for list %s" n ls))
   ((zerop n) (car ls))
   (t (list-ref (cdr ls) (sub1 n)))))


(defun list-ref (ls n)
  (cond
   ((<= (length ls) n)
    (error  "Index %d out of range for list %s" n ls))
   (t (list-ref-helper ls n))))

(defun list-ref-helper (ls n)
  (cond
   ((zerop n) (car ls))
   (t (list-ref-helper (cdr ls) (sub1 n)))))


(defun list-ref (ls n)
  (cond
   ((null ls)
    (error  "Index %d out of range for list %s" n ls))
   ((zerop n) (car ls))
   (t (list-ref (cdr ls) (sub1 n)))))

(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)
(list-ref '() 0)




;;; Ex3.1: sum

(defun sum (ls)
  (cond
   ((null ls) 0)
   (t (+ (car ls) (sum (cdr ls))))))

(sum '(1 2 3 4 5))
(sum '(6))
(sum '())


;;; Ex3.2: pairwise-sum
(defun pairwise-sum (n1 n2)
  (cond
   ((or (null n1) (null n2))
    '())
   (t (cons (+ (car n1) (car n2))
	    (pairwise-sum (cdr n1) (cdr n2))))))

(pairwise-sum '(1 3 2) '(4 -1 2))
(pairwise-sum '(3.2 1.5) '(6.0 -2.5))
(pairwise-sum '(7) '(11))
(pairwise-sum '() '())


(defun pairwise-product (n1 n2)
  (cond
   ((or (null n1) (null n2))
    '())
   (t  (cons (* (car n1) (car n2))
	     (pairwise-product (cdr n1) (cdr n2))))))

(pairwise-product '(1 2 3) '(1 2 3))
(pairwise-product '(5 6) '(7 8))
(pairwise-product '(6) '(6))
(pairwise-product '() '())


;;; Ex3.3: dot-product

(defun dot-product (n1 n2)
  (sum (pairwise-product n1 n2)))

(defun dot-product (n1 n2)
  (cond
   ((or (null n1) (null n2)) 0)
   (t (+ (* (car n1) (car n2))
	 (dot-product (cdr n1) (cdr n2))))))


(dot-product '(3 4 -1) '(1 -2 -3))
(dot-product '(0.003 0.035) '(8 2))
(dot-product '(5.3e4) '(2.0e-3))
(dot-product '() '())


;;; Ex3.4: mutl-by-n


(defun mult-by-n (num ntpl)
  (cond
   ((null ntpl) '())
   (t (cons (* (car ntpl) num)
	    (mult-by-n num (cdr ntpl))))))

(mult-by-n 3 '(1 2 3 4 5))
(mult-by-n 0 '(1 3 5 7 9 11))
(mult-by-n -7 '())

;;; Ex 3.5: index
(defun index (a ls)
  (cond
   ((member a ls)
    (index-helper a ls))
   (t -1)))

(defun member (a ls)
  (cond
   ((null ls) nil)
   ((equal (car ls) a) t)
   (t (member a (cdr ls)))))

(defun index-helper (a ls)
  (cond
   ((equal (car ls) a) 0)
   (t (add1 (index-helper a (cdr ls))))))

(index 3 '(1 2 3 4 5 6))
(index 'so '(do re me fa so la ti do))
(index 'a '(b c d e))
(index 'cat '())


;;Ex 3.6: make-list
(defun make-list (num a)
  (cond
   ((zerop num) '())
   (t (cons a (make-list (sub1 num) a)))))

(defun all-same? (ls)
  (cond
   ((null ls) t)
   ((null (cdr ls)) t)
   ((equal (car ls) (cadr ls))
    (all-same? (cdr ls)))
   (t nil)))

(make-list 5 'no)
(make-list 1 'maybe)
(make-list 0 'yes)
(length (make-list 7 'any))
(all-same? (make-list 100 'any))


;; Ex 3.7: count-background
(defun count-background (a ls)
  (cond
   ((null ls) 0)
   ((equal (car ls) a)
    (count-background a (cdr ls)))
   (t (add1 (count-background a (cdr ls))))))

(count-background 'blue '(red white blue yellow blue red))
(count-background 'red '(white blue green))
(count-background 'white '())


;; Ex 3.8: list-front



(defun list-front (ls num)
  (cond
   ((> num (length ls))
    (error "Error: Lenght of %s is less than %d" ls num))
   (t (list-front-helper ls num))))

(defun list-front-helper (ls num)
  (cond
   ((zerop num) '())
   (t (cons (car ls)
	    (list-front-helper (cdr ls) (sub1 num))))))


(list-front '(a b c d e f g) 4)
(list-front '(a b c) 4)
(list-front '(a b c d e f g) 0)
(list-front '() 3)

;; Ex 3.9
(defun wrapa (a num)
  (cond
   ((zerop num) a)
   (t (cons (wrapa a (sub1 num))
	    '()))))

(wrapa 'gift 1)
(wrapa 'sandwich 2)
(wrapa 'prisoner 5)
(wrapa 'moon 0)

;;Ex 3.10
;;remainder function in elisp
;: https://www.gnu.org/software/emacs/manual/html_node/eintr/Compute-a-Remaind
(% 7 5)
(% 6 3)
(% 0 0)
(% 1 0)
(% 0 5)

(defun multiple? (m n)
  (cond
   ((and (zerop m) (zerop n)) t)
   ((and (zerop n)) nil)
   (t (zerop (% m n)))))

(multiple? 7 2)
(multiple? 9 3)
(multiple? 5 0)
(multiple? 0 20)
(multiple? 17 1)
(multiple? 0 0)


;; Ex 3.11: sum-of-odds

(defun sum-of-odds (n)
  (cond
   ((zerop n) 0)
   (t (+ (sub1 (* 2 n))
	 (sum-of-odds (sub1 n))))))

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


;; Ex3.12
(defun n-tuple->integer (tup)
  (cond
   ((null tup)
    (error "Error: bad argument () to n-tuple->integer"))
   (t (ntup-integer-helper (sub1 (length tup)) tup))))

(defun ntup-integer-helper (pwr tup)
  (cond
   ((zerop pwr) (car tup))
   (t (+ (* (car tup) (expt 10 pwr) )
	 (ntup-integer-helper (sub1 pwr) (cdr tup))))))

(n-tuple->integer '(5))
(n-tuple->integer '(1 2))
(n-tuple->integer '(3 1 4 6))
(n-tuple->integer '(0))
(n-tuple->integer '())
(+ (n-tuple->integer '(1 2 3)) (n-tuple->integer '(3 2 1)))

;; Ex 3.13: please check answers in the corresponding shceme implementation file

;;; 3.3 Exact Arithmetic and Data Abstractions

(defun rzero? (rtl)
  (zerop (numr rtl)))

(defun r+ (x y)
  (make-ratl
   (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
   (* (denr x) (denr y))))

(defun r* (x y)
  (make-ratl
   (* (numr x) (numr y))
   (* (denr x) (denr y))))

(defun r- (x y)
  (make-ratl
   (- (* (numr x) (denr y)) (* (numr y) (denr x)))
   (* (denr x) (denr y))))


(defun rinvert (rtl)
  (if (rzero? rtl)
      (error "rinvert: Cannot invert %s" rtl)
    (make-ratl (denr rtl) (numr rtl))))

(defun r/ (x y)
  (r* x (rinvert y)))

(defun r= (x y)
  (= (* (numr x) (denr y))
     (* (numr y) (denr x))))


(defun rpositive? (rtl)
  (or (and (positive? (numr rtl)) (positive? (denr rtl)))
      (and (negative? (numr rtl)) (negative? (denr rtl)))))

(defun positive? (n)
  (> n 0))


(defun negative? (n)
  (< n 0))

(defun r> (x y)
  (rpositive? (r- x y)))

(defun r< (x y)
  (rpositive? (r- y x)))

(defun max (x y)
  (if (> x y)
      x
    y))

(defun rmax (x y)
  (if (r> x y)
      x
    y))

(defun rmin (x y)
  (if (r< x y)
      x
    y))

(defun extreme-value (pred x y)
  (if (funcall pred x y)
      x
    y))

(defun rmax (x y)
  (extreme-value (function r>) x y ))

(defun rmin (x y)
  (extreme-value (function r<) x y))

(defun max (x y)
  (extreme-value (function >) x y))


(defun min (x y)
  (extreme-value (function <) x y))


(max 5 3)
(min 5 3)

(defun rprint (rtl)
  (message "%s / %s" (numr rtl) (denr rtl)))

(defun numr (rtl)
  (car rtl))

(defun denr (rtl)
  (cdr rtl))

(defun make-ratl (int1 int2)
  (if (zerop int2)
      (error "make-ratl: The denominator cannot be zero.")
    (cons int1 int2)))


;; Ex 3.14 rminus

(defun rminus (rtl)
  (make-ratl (* -1 (numr rtl) (denr rtl))))


(rprint  (make-ratl 1 2))

;; Ex 3.15 same-sign

(defun rpositive? (rtl)
  (same-sign? (numr rtl) (denr rtl)))

(defun same-sign? (n1 n2)
    (or (and (positive? n1) (positive? n2))
	(and (negative? n1) (negative? n2))))

(defun rabs (rtl)
  (make-ratl (abs (numr rtl))
	     (abs (denr rtl))))

(rabs (make-ratl -1 -2)) 

