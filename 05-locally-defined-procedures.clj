; -*- mode: clojure;-*-

;;run clojure REPL without a project:  M-x cider-jack-in
;; C-x C-e evalute last expr

;; Chapter 5: Locally Defined Procedures


((fn [x y] (+ x y)) 2 3)

((fn [x]
   (fn [y]
     (- x y)) 15)
 20)

(let [a 2 b 3]
  (+ a b))

(let [a +
      b 3]
  (a 2 b))

(let [add2 (fn [x] (+ x 2))
      b (* 3 (/ 2 12))]
  (/ b (add2 b)))


(def a 5)

(+ 1 a)

(let [a 3]
  (+ 1 a))

(+ 1  a)

(let [a 5]
  (do
    (println (+ 1 a))
    (let [a 3]
      (println (+ 1 a)))
    (println (+ 1 a))))

(let [a 2 b 3]
  (+ a b))
;; is equivlent to 
((fn [a b] (+ a b)) 2 3)


(def a 10)
(def b 2)

(let [a (+ a 5)]
  (* a b))

(let [a 10 b 2]
  (let [a (+ a 5)]
    (* a b)))

(def addb (let [b  100]
            (fn [x]
              (+ x b))))

(let [b 10]
  (addb 25))

(let [b 2]
  (let [add2 (fn [x] (+ x b))
        b 0.5]
    (/ b (add2 b))))


(defn remove-leftmost [item ls]
  (cond
    (empty? ls) '()

    (= (first ls) item)
    (rest ls)

    (and (list? (first ls))
         (not (empty? (first ls))))
    (let [rem-list (remove-leftmost item (first  ls))]
      (cons rem-list (cond
                       (= (first ls) rem-list)
                       (remove-leftmost item (rest ls))
                       :else (rest ls))))

    :else (cons (first ls)
                (remove-leftmost item (rest ls)))))

(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))

(defn sub1 [n]
  (- n 1))

;; the following code will result in error:
;; "Unable to resolve symbol: fact in this context"
;; This message refers to the fact occuring in the lambda expression,
;;which is not bound outside of the let expression.
(let [fact (fn [n]
             (if (zero? n)
               1
               (* n (fact (sub1 n)))))]
  (fact 4))


(letfn [(fact [n]
          (if (zero? n)
            1
            (* n (fact (sub1 n)))))]
  (fact 4))


;;Mutual recursion
;;dec simlar to sub1
(letfn [(even? [x]
          (or (zero? x) (odd? (dec x))))
        (odd? [x]
          (or (not (zero? x)) (even? (dec x))))]
  (odd? 17))


(defn fact [n]
  (letfn [(fact-it [k acc]
            (if (zero? k)
              acc
              (fact-it (dec k) (* k acc))))]
    (fact-it n 1)))

(fact 4)


(defn swapper [x y ls]
  (letfn
      [(swap [ls*]
         (cond
           (empty? ls*) '()
           (= (first ls*) x) (cons y (swap (rest ls*)))
           (= (first ls*) y) (cons x (swap (rest ls*)))
           :else (cons (first ls*) (swap (rest ls*)))))]
    (swap ls)))

(swapper 'cat 'dog '(my cat eats dog food))
(swapper 'john 'mary '(john loves mary))
(swapper 'a 'b  '(c (a b) d))
(swapper 'a 'b '())
(swapper 'b 'd '(a b c d b))


;;Ex5/.1
(let [a 5]                      ;Env1 
  (let [fun (fn [x] (max x a))] ;Env2, a bound to 5 in Env1, x bound to 1 from fn param
    (let [a 10                  ;Env3
          x 20]
      (fun 1))))

;; Env1: a = 5
;; Env2: fun -> x , (max x a=5) , Env1
;; Env3: a = 10, x = 20


(let [a 1 b 2] ;Env1
  (let [b 3 c (+ a b)] ;Env2
    (let [b 5] ;Env3
      (cons a (cons b (cons c '()))))))

;; => (1 5 4)
;; Env1: a=1 b=2
;; Env2: b = 3, c  = (+ a=1 b=3) = 4
;; Env3: b=5 (cons a=1 (cons b=5 (cons c=4 '())))
;;Note the differnce betwen let evaluation order in scheme vs Clojure
;;in Clojure, the bindings in a let form are evaluated in the order they are written, and each binding can depend on the previous ones. This means you can use the value of a previously defined binding in the definition of a subsequent binding.
(let [x 2
      y (+ x 3)
      z (* y 2)]
  [x y z])  ;; Returns [2 5 10]


;;Ex5.2
;;a
(letfn
    [(loop [n k]
       (cond
         (zero? k) n
         (< n k) (loop k n)
         :else (loop k (rem n k))))]
    (loop 9 12))
;; This is GCD function
;; (loop 9 12)
;; (loop 12 9) ; (< 9 12)
;; (loop 9 (remainder 12 9)) -> (loop 9 3)
;; (loop 3 (remainder 9 3)) -> (loop 3 0)
;; 3 (zero? k=0)

;;b
(letfn
    [(loop [n]
       (if (zero? n)
         0
         (+ (rem n 10)
            (loop (quot n 10)))))]
  (loop 1234))
;;;;calculates the sum of the digits of a given number

;;(loop 1234)
;;(+  4 (loop 123))
;;(loop 123)
;;(+ 3 (loop 12))
;;(loop 12)
;;(+ 2 (loop 1))
;;(loop 1)
;;(+1 (loop 0))
;;


;;Ex5.3
(let [a 2 b 3]
  (+ a b))
;; is equivlent to 
((fn [a b] (+ a b)) 2 3)


((fn [a b]
   ((fn [b c]
      ((fn [b]
         (cons a (cons b (cons c '()))))
       5))3 (+ a b))) 1 2)

((fn [a]
   ((fn [fun]
      ((fn [a x]
         (fun 1))
       10 20))
    (fn [x] (max x a))))
 5)

;;=================
;;Ex 5.4


(defn append [ls1 ls2]
  (if (empty? ls1)
    ls2
    (cons (first ls1) (append (rest ls1) ls2))))

(defn even? [int]
  (if (zero? int)
    true
    (odd? (dec int))))

(defn odd? [int]
  (if (zero? int)
    false
    (even? (dec int))))

;;============
;;Ex5.4


(letfn [(mystery [tuple odds evens]
          (if (empty? tuple)
            (append odds evens)
            (let [next-int (first tuple)]
              (if (odd? next-int)
                (mystery (rest tuple)
                         (cons next-int odds) evens)
                (mystery (rest tuple)
                         odds (cons next-int evens))))))]
  (mystery '(3 16 4 7 9 12 24) '() '()))

;;Create a list where odds comes first then evens
;;=> (9 7 3 24 12 4 16)

;;=============
;;Ex5.5

(defn mystery [n]
  (letfn
      [(mystery-helper [n s]
         (cond
           (zero? n) (list s)

           :else (append
                  (mystery-helper (dec n) (cons 0 s))
                  (mystery-helper (dec n) (cons 1 s)))))]
    (mystery-helper n '())))

(mystery 4);;All binary numbers of 4 bits = 2^4 = 16
;; => ((0 0 0 0) (1 0 0 0) (0 1 0 0) (1 1 0 0) (0 0 1 0) (1 0 1 0) (0 1 1 0) (1 1 1 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (1 0 1 1) (0 1 1 1) (1 1 1 1))

(mystery 3);;All binary numbers of 3 bits = 2^3 = 8
;; => ((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1))

;;(mystery n) All binary numbers of n bits = 2^n


;;Ex5.6
(defn insert-left-all [new old ls]
  (letfn
      [(insert-la [ls]
         (cond
           (empty? ls) '()

           (= (first ls) old)
           (cons new (cons old (insert-la (rest ls))))

           (list? (first ls))
           (cons (insert-la (first ls))
                 (insert-la (rest ls)))

           :else (cons (first ls)
                       (insert-la (rest ls)))))]
      (insert-la ls)))

(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())

;;Ex5.7
(defn fib [n]
  (letfn
      [(fib-it [n acc1 acc2]
         (if (= n 1)
           acc2
           (fib-it (dec n) acc2 (+ acc1 acc2))))]
    (fib-it n 0 1)))

(fib 4)


;;Ex5.8

(defn length [ls]
  (if (empty? ls)
    0
    (inc (length (rest ls)))))

(length '(1 2 3 4))
(length '())

(defn list-ref [ls n]
  (letfn
      [(list-ref-helper [ls n]
         (if (zero? n)
           (first ls)
           (list-ref-helper (rest ls) (dec n))))]
      (if (<= (length ls) n)
        (throw (Exception. (str "list-ref: Index " n " out of range for list " ls)))
        (list-ref-helper ls n))))

(list-ref '(a b c d e f) 3)
(list-ref '(a b c d e f) 0)
(list-ref '(a b c) 3)
(list-ref '((1 2) (3 4) (5 6)) 1)
(list-ref '() 0)

