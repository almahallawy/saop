; -*- mode: clojure;-*-

;;run clojure REPL without a project:  M-x cider-jack-in
;; C-x C-e evalute last expr

;; Chapter 4: Data Driven Recursion
(defn append [ls1 ls2]
  (if  (empty? ls1)
      ls2
    (cons (first ls1) (append (rest ls1) ls2))))


(append '(a b c) '(c d))
(append '() '(a b c))

(defn reverse [ls]
  (if (empty? ls)
    []
    (append (reverse (rest ls)) (list (first ls)))))


(reverse '(1 2 3 4 5))
(reverse '((1 2) (3 4) (5 6)))

(defn merge [tup1 tup2]
  (cond
    (empty? tup1) tup2

    (empty? tup2) tup1

    (< (first tup1) (first tup2))
    (cons (first tup1)
          (merge (rest tup1) tup2))

    :else (cons (first tup2)
                (merge tup1 (rest tup2)))))

(merge '(2.3 4.7 5 8.1) '(1.7 4.7))

(defn sub1 [n]
  (- n 1))


(defn even? [int]
  (if (zero? int)
    true
    (odd? (sub1 int))))

(defn odd? [int]
  (if (zero? int)
    false
    (even? (sub1 int))))

(even? 10)
(even? 9)
(odd? 10)
(odd? 9)
(odd? 0)
(even? 0)

(defn remove [item ls]
  (cond
    (empty? ls) '()

    (=  (first ls) item)
    (remove item (rest ls))

    :else (cons (first ls)
                (remove item (rest ls)))))

(remove  4' (1 4 2 3 4 5 6 7 4))


;EX4.1
(defn insert-left [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons new (cons old (insert-left new old (rest ls))))

    :else (cons (first ls)
                (insert-left new old (rest ls)))))

(insert-left 'z 'a '(a b a c a))
(insert-left 0 1 '(0 1 0 1))
(insert-left 'dog 'cat '(my dog is fun))
(insert-left 'two 'one '())


;;Ex4.2
(defn insert-right [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons old (cons new (insert-right new old (rest ls))))

    :else (cons (first ls)
                (insert-right new old (rest ls)))))


(insert-right 'z 'a '(a b a c a))
(insert-right 0 1 '(0 1 0 1))
(insert-right 'dog 'cat '(my dog is fun))
(insert-right 'two 'one '())

;;Ex subst
(defn subst [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons new (subst new old (rest ls)))

    :else (cons (first ls)
                (subst new old (rest ls)))))


(subst 'z 'a '(a b a c a))
(subst 0 1 '(0 1 0 1))
(subst 'dog 'cat '(my dog is fun))
(subst 'two 'one '())


;;Ex deepen-1

(defn deepen-1 [ls]
  (if (empty? ls)
    '()
    (cons (list (first ls))
          (deepen-1 (rest ls)))))

(deepen-1 '(a b c d))
(deepen-1 '((a b) (c (d e)) f))
(deepen-1 '())


(defn add1 [n]
  (+ 1 n))

;;Implement similar to scheme code in book
;;by treating '() as atomic
(defn count-all [ls]
  (cond
    (empty? ls) 0

    (or (not (list? (first ls)))
        (empty? (first ls)))
    (add1 (count-all (rest ls)))

    :else (+ (count-all (first ls))
             (count-all (rest ls)))))

(defn count-all [ls]
  (cond
    (empty? ls) 0

    :else (+ (if (and (list? (first ls))
                      (not (empty? (first ls))))
               (count-all (first ls))
               1)
             (count-all (rest ls)))))

(defn count-all [ls]
  (cond
    (empty? ls) 0

    (and (list? (first ls))
         (not (empty? (first ls))))
    (+ (count-all (first ls))
       (count-all (rest ls)))

    :else (+ 1 (count-all (rest ls)))))


(count-all '((a b) c () ((d (e)))))
(count-all '(() () ()))
(count-all '((())))
(count-all '())



(defn remove-all [item ls]
  (cond
    (empty? ls) '()

    (= (first ls) item)
    (remove-all item (rest ls))

    (list? (first ls))
    (cons (remove-all item (first ls))
          (remove-all item (rest ls)))

    :else (cons (first ls)
                (remove-all item (rest ls)))))

(defn remove-all [item ls]
  (cond
    (empty? ls) '()

    (= (first ls) item)
    (remove-all item (rest ls))

    :else (cons (if (list? (first ls))
                  (remove-all item (first ls))
                  (first ls))
                (remove-all item (rest ls)))))

(remove-all 'a '((a b (c a)) (b (a c) a)))


(defn reverse-all [ls]
  (cond
    (empty? ls) '()

    (seq? (first ls))
    (append (reverse-all (rest ls))
            (list (reverse-all (first ls))))

    :else (append (reverse-all (rest ls))
                  (list (first ls)))))

(defn reverse-all [ls]
  (if (empty? ls)
    '()
    (append (reverse-all (rest ls))
            (list (if (seq? (first ls))
                    (reverse-all (first ls))
                    (first ls))))))

(reverse-all '(a (b c) (d (e f))))
(reverse-all '(a (b c)))


;;Ex 4.5

(defn subst-all [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons new (subst-all new old (rest ls)))

    (seq? (first ls))
    (cons (subst-all new old (first ls))
          (subst-all new old (rest ls)))

    :else (cons (first ls)
                (subst-all new old (rest ls)))))


(defn subst-all [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons new (subst-all new old (rest ls)))

    :else (cons (if (seq? (first ls))
                  (subst-all new old (first ls))
                  (first ls))
                (subst-all new old (rest ls)))))


(subst-all 'z 'a '(a (b (a c)) (a (d a))))
(subst-all 0 '(1) '(((1) (0))))
(subst-all 'one 'two '())


;; Ex 4.6

(defn insert-left-all [new old ls]
  (cond
    (empty? ls) '()

    (= (first ls) old)
    (cons new (cons old
                    (insert-left-all new old (rest ls))))

    :else (cons (if (seq? (first ls))
                  (insert-left-all new old (first ls))
                  (first ls))
                (insert-left-all new old (rest ls)))))

(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))
(insert-left-all 'z 'a '())

;;Ex4.7

(defn sum-all [ls]
  (cond
    (empty? ls) 0

    :else (+ (if (seq? (first ls))
               (sum-all (first ls))
               (first ls))
             (sum-all (rest ls)))))


(sum-all '((1 3) (5 7) (9 11)))
(sum-all '(1 (3 (5 (7 (9))))))
(sum-all '())


(defn depth [item]
  (if (or (not (seq? item)) (empty? item))
    0
    (max (+ 1 (depth (first item)))
         (depth (rest item)))))

(depth '(a (b c d) ((e f) g)))

(depth '(() (b c d) ((e f) g)))

(defn flatten [ls]
  (cond
    (empty? ls) '()

    (and (seq? (first ls)) (not (empty? (first ls))))
    (append (flatten (first ls))
            (flatten (rest ls)))

    :else (cons (first ls)
                (flatten (rest ls)))))

(flatten '(a (b c d) ((e f) g)))

(flatten '(() (b c d) ((e f) g)))

(defn member-all? [item ls]
  (cond
    (empty? ls) false

    (= (first ls) item) true

    (list? (first ls))
    (or (member-all? item (first ls))
        (member-all? item (rest ls)))

    :else (member-all? item (rest ls))))

(member-all? 'b '(a (b c) (c (b a))))
(member-all? '(c d) '((a (b c)) ((c d) e)))

(defn remove-leftmost [item ls]
  (cond
    (empty? ls) '()

    (= (first ls) item)
    (rest ls)

    (or (not (list? (first ls))) (empty? (first ls)))
    (cons (first ls)
          (remove-leftmost item (rest ls)))

    (member-all? item (first ls))
    (cons (remove-leftmost item (first ls)) (rest ls))

    :else (cons (first ls)
                (remove-leftmost item (rest ls)))))


(defn remove-leftmost [item ls]
  (cond
    (empty? ls) '()

    (= (first ls) item)
    (rest ls)

    (and (list? (first ls)) (member-all? item (first ls)))
    (cons (remove-leftmost item (first ls))
          (rest ls))

    :else (cons (first ls)
                (remove-leftmost item (rest ls)))))

(remove-leftmost 'b '(a (b c) (c (b a))))

(remove-leftmost '(c d) '((a (b c)) ((c d) e)))


;;Ex4.8
(list? '())

(defn count-parens-all [ls]
  (cond
    (empty? ls) 2

    (list? (first ls))
    (+ (count-parens-all (first ls))
       (count-parens-all (rest ls)))

    :else (count-parens-all (rest ls))))


(count-parens-all '())
(count-parens-all '((a b) c))
(count-parens-all '(((a () b) c) () ((d) e)))
(count-parens-all '(() ()))


;;Ex4.9
(defn count-background-all [item ls]
  (cond
    (empty? ls) 0

    (list? (first ls))
    (+ (count-background-all item (first ls))
       (count-background-all item (rest ls)))

    (= (first ls) item)
    (count-background-all item (rest ls))

    :else (+ 1 (count-background-all item (rest ls)))))


(count-background-all 'a '((a) b (c a) d))
(count-background-all 'a '((((b (((a)) c))))))
(count-background-all 'b '())


;;Ex4.10
(defn leftmost [ls]
  (cond
    (empty? ls) '()

    (not (list? (first ls)))
    (first ls)
    
    :else (leftmost (first ls))))

(leftmost '((a b) (c (d e))))
(leftmost '((((c ((e f) g) h)))))
(leftmost '(() a))
(leftmost '())

;;lef 4.11
(defn rightmost [ls]
  (cond
    (empty? ls) '()

    (empty? (rest ls))
    (if (list? (first ls))
      (rightmost (first ls))
      (first ls))
    
    :else (rightmost (rest ls))))

(defn rightmost [ls]
  (cond
    (empty? ls) '()

    (and (list? (rest ls)) (not (empty? (rest  ls))))
    (rightmost (rest ls))

    (and (list? (first ls)) (not (empty? (first ls))))
    (rightmost (first ls))

    :else (first ls)))

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v))
(rightmost '(((((b (c)))))))
(rightmost '(a ()))
(rightmost '(()))


(defn sub1 [n]
  (- n 1))

(sub1 3)


(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (sub1 n)))))


(fact 5)
(fact 3)

(defn fact-it [n acc]
  (if (zero? n)
    acc
    (fact-it (sub1 n) (* n acc))))

(fact-it 3 1)


(defn fact [n]
  (fact-it n 1))

(fact 5)
(fact 3)

;;EX 4.14

(defn harmonic-sum [n]
  (cond
    (zero? n) 0
    :else (+ (/ 1 n) (harmonic-sum (sub1 n)))))

(harmonic-sum 4)

(defn harmonic-sum-it [n acc]
  (if (zero? n)
    acc
    (harmonic-sum-it (sub1 n) (+ (/ 1 n) acc))))

(Math/log 10)
(harmonic-sum-it 10 0)

(Math/log 100)
(harmonic-sum-it 100 0)

;;
(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 3)
(fib 5)0

(defn fib-it [n acc1 acc2]
  (if (= n 1)
    acc2
    (fib-it (sub1 n) acc2 (+ acc1 acc2))))

(fib-it 6 0 1)

(defn fib [n]
  (if (zero? n)
    0
    (fib-it n 0 1)))

(fib 6)


(defn fib-it [n acc1 acc2]
  (if (zero? n)
    acc1
    (fib-it (sub1 n) acc2 (+ acc1 acc2))))

(fib-it 1 0 1)
(fib-it 6 0 1)
(fib-it 0 0 1)

(defn fib [n]
  (fib-it n 0 1))


(fib 0)
(fib 1)
(fib 2)
(fib 6)


;;Ex 4.15
(defn fib [n]
  (println "n = " n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0)
(fib 1)
(fib 4)
(fib 5)
(fib 6)



;;Ex 4.16
(defn fib-it [n acc1 acc2]
  (println "n = "n" , acc1 = "acc1" , acc=2 = "acc2)
  (if (= n 1)
    acc2
    (fib-it (sub1 n) acc2 (+ acc1 acc2))))


(fib-it 1 0 1)
(fib-it 4 0 1)


;;Ex 4.17

(defn add1 [n]
  (+ n 1))

(defn sub1 [n]
  (- n 1))

(defn call-fib [n]
  (add1 (* 2 (sub1 (fib (add1 n))))))

(defn adds-fib [n]
  (sub1 (fib (add1 n))))

(call-fib 0)
(adds-fib 0)
(call-fib 1)
(adds-fib 1)
(call-fib 2)
(adds-fib 2)
(call-fib 3)
(adds-fib 3)
(call-fib 4)
(adds-fib 4)
(call-fib 5)
(adds-fib 5)

;;Ex4.18

(defn length [ls]
  (if (empty? ls)
    0
    (add1 (length (rest ls)))))

(length '(1 2 3 4 5))


(defn length-it [ls acc]
  (if (empty? ls)
    acc
    (length-it (rest ls) (add1 acc))))

(length-it '(1 2 3 4 5) 0)

;;Ex 4.19
(defn mk-asc-list-of-ints [n]
  (if (zero? n)
    '()
    (append (mk-asc-list-of-ints (sub1 n)) (list n))))

(mk-asc-list-of-ints 5)

(defn mk-asc-list-of-ints [n acc]
  (if (zero? n)
    acc
    (mk-asc-list-of-ints (sub1 n) (cons n acc))))

(mk-asc-list-of-ints 6 '())


(defn mk-desc-list-of-ints [n]
  (if (zero? n)
    '()
    (cons n (mk-desc-list-of-ints (sub1 n)))))

(mk-desc-list-of-ints 6)


(defn mk-desc-list-of-ints [n acc]
  (if (zero? n)
    acc
    (mk-desc-list-of-ints (sub1 n) (append acc (list n)))))


(mk-desc-list-of-ints 6 '())


;;Ex 4.20
(defn occurs [ls item]
  (cond
    (empty? ls) 0
    
    (= (first ls) item)
    (add1 (occurs (rest ls) item))

    :else (occurs (rest ls) item)))

(occurs '(a b a c a d) 'a)
(occurs '(b c a (b a) c a) 'a)
(occurs '(b (c d)) 'a)

(defn occurs-it [ls item  acc]
  (cond
    (empty? ls) acc

    (= (first ls) item)
    (occurs-it (rest ls) item (add1 acc))

    :else (occurs-it (rest ls) item acc)))

(occurs-it '(a b a c a d) 'a 0)
(occurs-it '(b c a (b a) c a) 'a 0)
(occurs-it '(b (c d)) 'a 0)
