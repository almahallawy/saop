; -*- mode: clojure;-*-

;;run clojure REPL without a project:  M-x cider-jack-in
;; C-x C-e evalute last expr

;; Chapter 3 - Data Abstractions and Numbers


(defn add1 [n]
  (+ n 1))

(defn sub1 [n]
  (- n 1))


(defn harmonic-sum [n]
  (cond
    (zero? n) 0
    :else (+ (/ 1 n) (harmonic-sum (sub1 n)))))


(defn list-of-zeros [n]
  (cond
    (zero? n) '()
    :else (cons 0 (list-of-zeros (sub1 n)))))

(list-of-zeros 5)


(defn length [ls]
  (if (empty? ls)
    0
    (add1 (length (rest ls)))))


(length '(a b c d e))
(length '(1 (2 3) (4 5 6)))
(length '())

;;Clojure has count
(count '(1 2 3))
(count '())
(count "hello")


(defn list-ref [ls n]
  (cond
    (<= (length ls) n)
    (throw (Exception. (str "list-ref: Index " n " out of range for list " ls)))

    (zero? n) (first ls)

    :else (list-ref (rest ls) (sub1 n))))



(defn list-ref-helper [ls n]
  (if (zero? n)
    (first ls)
    (list-ref-helper (rest ls) (sub1 n))))

(defn list-ref [ls n]
  (if (<= (length ls) n)
    (throw (Exception. (str "list-ref: Index " n " out of range for list " ls)))
    (list-ref-helper ls n)))


(defn list-ref [ls n]
  (cond
    (empty? ls)
    (throw (Exception. (str "list-ref: Index " n " out of range for list " ls)))

    (zero? n) (first ls)

    :else (list-ref (rest ls) (sub1 n))))

(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)
(list-ref '() 0) ()


;;Ex3.1

(defn sum [ls]
  (cond
    (empty? ls) 0
    :else (+ (first ls) (sum (rest ls)))))


(sum '(1 2 3 4 5))
(sum '(6))
(sum '())


;;Ex3.2


(defn pairwise-sum [ntp1 ntp2]
  (if (empty? ntp1)
    '()
    (cons (+ (first ntp1) (first ntp2))
          (pairwise-sum (rest ntp1) (rest ntp2)))))


(pairwise-sum '(1 3 2) '(4 -1 2))
(pairwise-sum '(3.2 1.5) '(6.0 -2.5))
(pairwise-sum '(7) '(11))
(pairwise-sum '() '())


(defn pairwise-product [ntp1 ntp2]
  (if (empty? ntp1)
    '()
    (cons (* (first ntp1) (first ntp2))
          (pairwise-product (rest ntp1) (rest ntp2)))))


(pairwise-product '(1 2 3) '(1 2 3))
(pairwise-product '(5 6) '(7 8))
(pairwise-product '(6) '(6))
(pairwise-product '() '())


;;Ex3.3
(defn dot-product [ntp1 ntp2]
  (sum (pairwise-product ntp1 ntp2)))

(defn dot-product [ntp1 ntp2]
  (if (empty? ntp1)
    0
    (+ (* (first ntp1) (first ntp2))
       (dot-product (rest ntp1) (rest ntp2)))))




(dot-product '(3 4 -1) '(1 -2 -3))
(dot-product '(0.003 0.035) '(8 2))
(dot-product '(5.3e4) '(2.0e-3))
(dot-product '() '())


;;Ex3.4
(defn mult-by-n [num ntpl]
  (if (empty? ntpl)
    '()
    (cons (* (first ntpl) num)
          (mult-by-n num (rest ntpl)))))

(mult-by-n 3 '(1 2 3 4 5))
(mult-by-n 0 '(1 3 5 7 9 11))
(mult-by-n -7 '())


;;Ex3.5
(defn index [a ls]
  (cond
    (empty? ls)  -1

    (= (first ls) a) 0

    :else (if (= (index a (rest ls)) -1)
            -1
            (add1 (index a (rest ls))))))

(index 3 '(1 2 3 4 5 6))
(index 'so '(do re me fa so la ti do))
(index 'a '(b c d e))
(index 'cat '())

;;(index a ntpl) = (sub1 (index a (rest ntp1)))

;;Ex3.6
(defn all-same? [ls]
  (cond
    (empty? ls) true
    (empty? (rest ls)) true
    (= (first ls) (first (rest ls))) (all-same? (rest (rest ls)))
    :else false))


(defn make-list [num a]
  (if (zero? num)
    '()
    (cons a (make-list (sub1 num) a))))


(make-list 5 'no)
(make-list 1 'maybe)
(make-list 0 'yes)
(length (make-list 7 'any))
(all-same? (make-list 100 'any))


;;Ex3.7


(defn count-background [a ls]
  (cond
    (empty? ls) 0

    (= (first ls) a) (count-background a (rest ls))

    :else (add1 (count-background a (rest ls)))))

(count-background 'blue '(red white blue yellow blue red))
(count-background 'red '(white blue green))
(count-background 'white '())

;;Ex3.8
(defn list-front [ls num]
  (cond
    (<= (length ls) num)
    (throw (Exception. (str "Error: length of " ls " is less than " num)))

    (zero? num) '()

    :else (cons (first ls) (list-front (rest ls) (sub1 num)))))

;;----Using the helper
(defn list-front-helper [ls num]
  (if (zero? num)
    '()
    (cons (first ls) (list-front-helper (rest ls) (sub1 num)))))

(defn list-front [ls num]
  (if (<= (length ls) num)
    (throw (Exception. (str "Error: length of " ls " is less than " num)))
    (list-front-helper ls num)))

(list-front '(a b c d e f g) 4)
(list-front '(a b c) 4)
(list-front '(a b c d e f g) 0)
(list-front '() 3)

;;Ex3.9

(defn wrapa [a num]
  (if (zero? num)
    a
    (cons (wrapa a (sub1 num)) '())))

(wrapa 'gift 1)
(wrapa 'sandwich 2)
(wrapa 'prisoner 5)
(wrapa 'moon 0)

;;;; Exercise 3.10: multiple?


(defn multiple? [m n]
  (cond
    (and (zero? m) (zero? n)) true
    (zero? m) true
    (zero? n) false
    :else (zero? (rem m n))))

(multiple? 7 2)
(multiple? 9 3)
(multiple? 5 0)
(multiple? 0 20)
(multiple? 17 1)
(multiple? 0 0)

;;Ex3.11

;;(sum-of-odds 0) = 0
;;(sum-of-odds 1) = (2 * 1 - 1) + (sum-of-odds 0) = 1 + 0 = 1
;;(sum-of-odds 2) = (2 * 2 - 1) + (sum-of-odds 1) = 3 + 1 = 4
;;(sum-of-odds 3) = (2 * 3 - 1) + (sum-of-odds 2) = 5 + 4 = 9
;;(sum-of-odds n) = (2 * n-1) + (sum-of-odds n-1)

(defn sum-of-odds [n]
  (if (zero? n) 0
      (+ (sub1 (* 2 n))
         (sum-of-odds (sub1 n)))))

(sum-of-odds 0)
(sum-of-odds 1)
(sum-of-odds 2)
(sum-of-odds 3)
(sum-of-odds 4)
(sum-of-odds 5)
(sum-of-odds 6)
(sum-of-odds 7)
(sum-of-odds 8)
(sum-of-odds 9)
(sum-of-odds 10)

;;Ex3.12

(defn n-tuple->integer [ls]
  (cond
    (empty? ls) (throw (Exception. (str "Error: bad argument" ls " to n-tuple->integer:")))
    (empty? (rest ls)) (first ls)
    :else (+ (* (first ls) (Math/pow 10 (sub1(length ls))))
             (n-tuple->integer (rest ls)))))


(n-tuple->integer '(5))
(n-tuple->integer '(1 2))
(n-tuple->integer '(3 1 4 6))
(n-tuple->integer '(0))
(n-tuple->integer '())
(+ (n-tuple->integer '(1 2 3)) (n-tuple->integer '(3 2 1)))


;;Exact Arithmetic and Data Abstraction

(defn numr [rtl]
  (first rtl))

(defn denr [rtl]
  (first (rest rtl)))

(defn make-ratl [int1 int2]
  (if (zero? int2)
    (throw (Exception.  "make-ratl: The denominator cannot be zero."))
    (list int1 int2)))



(defn rzero? [rtl]
  (zero? (numr rtl)))

(defn r+ [x y]
  (make-ratl
   (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
   (* (denr x) (denr y))))

(defn r* [x y]
  (make-ratl
   (* (numr x) (numr y))
   (* (denr x) (denr y))))

(defn r- [x y]
  (make-ratl
   (- (* (numr x) (denr y)) (* (numr y) (denr x)))
   (* (denr x) (denr y))))

(defn rinvert [rtl]
  (if (rzero? rtl)
    (throw (Exception.  (str "rinvert: Cannot invert " rtl)))
    (make-ratl (denr rtl) (numr rtl))))


(defn r-division [x y]
  (r* x (rinvert y)))

(defn r= [x y]
  (= (* (numr x) (denr y)) (* (numr y) (denr x))))

(defn rpositive? [rtl]
  (or (and (pos? (numr rtl)) (pos? (denr rtl)))
      (and (neg? (numr rtl)) (neg? (denr rtl)))))

(defn r> [x y]
  (rpositive? (r- x y)))


(defn r< [x y]
  (rpositive? (r- y x)))


;;use max in clojure instead
;; (defn max [x y]
;;   (if (> x y)
;;     x
;;     y))

(defn rmax [x y]
  (if (r> x y)
    x
    y))

(defn rmin [x y]
  (if (r< x y)
    x
    y))

(defn extreme-value [pred x y]
  (if (pred x y)
    x
    y))


(defn rmax [x y]
  (extreme-value r> x y))

(defn rmin [x y]
  (extreme-value r< x y))

(defn rprint [rtl]
  (println (numr rtl) "/" (denr rtl)))


;;Ex 3.14

(defn rminus [rtl]
  (make-ratl (* -1 (numr rtl)) (denr rtl)))


(def rr (make-ratl 1 2))
(rminus rr)


;;Ex 3.15
(defn same-sign? [x y]
  (or (and (pos? x) (pos? y))
      (and (neg? x) (neg? y))))

(defn rpositive? [rtl]
  (same-sign? (numr rtl) (denr rtl)))

(rpositive? (make-ratl -1 -2))


;;Ex3.16

(defn rabs [rtl]
  (make-ratl (abs (numr rtl)) (abs (denr rtl))))


(rabs (make-ratl 1 -2))

;;Ex3.17


(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(gcd 8 12)
(gcd 8 -12)
(gcd 0 5)
(gcd 12 15)
(gcd 7 9)
(gcd 0 8)


(defn make-ratl [int1 int2]
  (if (zero? int2)
    (throw (Exception.  "make-ratl: The denominator cannot be zero."))
    (list (/ int1 (abs (gcd int1 int2)))
          (/ int2 (abs (gcd int1 int2))))))


(make-ratl 24 30)
(make-ratl -10 15)
(make-ratl 8 -10)
(make-ratl -6 -9)
(make-ratl 0 8)


