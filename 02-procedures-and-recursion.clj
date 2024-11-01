; -*- mode: clojure;-*-

;;run clojure REPL without a project:  M-x cider-jack-in
;; C-x C-e evalute last expr

;; Chapter 2 - Procedure and Recursion

;; In clojure anonymous function similar to scheme/lambda is fn or #()

(fn [item]  (cons item))
((fn [item]  (cons item '())) 'bit)
((fn [item]  (cons item '())) (* 5 6))



(defn make-list-of-one [item]
  (cons item '()))

(make-list-of-one 'bit)

(defn make-list-of-two [item1 item2]
  (cons item1 (make-list-of-one item2)))

(make-list-of-two 'one 'two)


(defn regroup [list-of-4]
  (make-list-of-two
   (first-group list-of-4)
   (second-group list-of-4)))

(defn first-group [ls]
  (make-list-of-two (first ls) (first (rest ls))))

(defn second-group [ls]
  (rest (rest ls)))

(regroup '(chicken soup ice cream))

(list 'a 'b'c 'd)
(list '(1 2) '(3 4))
(list)

;;;; Exercise 2.1: second
(defn second [ls]
  (first (rest ls)))


(second '(1 2 3))


;;;; Exercise 2.2: third
(defn third [ls]
  (first (rest (rest ls))))

(third '(1 2 3 4))



;; Ex 2.3
(defn first-of-both [ls1 ls2]
  (make-list-of-two (first ls1) (first ls2)))

(first-of-both '(1 3 5 7) '(2 4 6))
(first-of-both '((a b) (c d)) '((e f) (g h)))

;;Ex2.4

(defn juggle [ls]
  (cons (third ls)
        (make-list-of-two (first ls) (third ls))))


(juggle '(jump quick spot))
(juggle '(dog bites man))
  
;;Ex2.5

(defn switch [ls]
  (cons (third ls)
        (make-list-of-two (second ls) (first ls))))


(switch '(jump quick spot))
(switch '(dog bites man))
 
;;;;;;;;;;;;;;;;


(defn car-if-pair [item]
  (cond
    (list? item) (first item)
    :else item))

(car-if-pair 5)
(car-if-pair '(a b))

(defn singleton-list? [ls]
  (and (list? ls) (empty? (rest ls))))

(singleton-list? '(cat))

(defn s-and-n-list? [ls]
  (and (list? ls)
       (symbol? (first ls))
       (list? (rest ls))
       (number? (first (rest ls)))))

(s-and-n-list? '(a 1 b))
(s-and-n-list? '(a b 1))


(defn s-or-n-list? [ls]
  (and (list? ls)
       (or (symbol? (first ls))
           (number? (first ls)))))


;;;; Exercise 2.8
(s-and-n-list? '(2 pair 12 dozen))
(s-and-n-list? '(b 4 u c a j))
(s-and-n-list? '(a ten))
(s-and-n-list? '(a))

;;;; Exercise 2.9
(s-or-n-list? '(b))
(s-or-n-list? '(c 2 m))
(s-or-n-list? '(10 10 10 10))
(s-or-n-list? '())


(defn last-item [ls]
  (cond
    (empty? (rest ls)) (first ls)
    :else (last-item (rest ls))))

(last-item '(a b c))

(defn member? [item ls]
  (cond
    (empty? ls) false
    :else (or (= (first ls) item)
              (member? item (rest ls)))))


(member? 'cat '(dog hen cat pig))
(member? 'fox '(dog hen cat pig))
(member? 2 '(1 (2 3) 4))
(member? '(2 3) '(1 (2 3) 4))
(member? 'cat '())


(defn remove-1st [item ls]
  (cond
    (empty? ls) '()
    (= (first ls) item) (rest ls)
    :else (cons (first ls)
                (remove-1st item (rest ls)))))

(remove-1st 'fox '(hen fox chick cock))
(remove-1st 'fox '(hen fox chick fox cock))
(remove-1st 'fox '(hen (fox chick) cock))
(remove-1st 'fox '())
(remove-1st '(1 2) '(1 2 (1 2) ((1 2))))

;;Ex2.10

(defn last-item [ls]
  (if (empty? (rest ls))
    (first ls)
    (last-item (rest ls))))

(last-item '(a b c))

(defn member? [item ls]
  (if (empty? ls)
    false
    (or (= (first ls) item)
        (member? item (rest ls)))))

;;Ex2.11

(defn member? [item ls]
  (cond
    (empty? ls) false
    (= (first ls) item) true
    :else (member? item (rest ls))))


;;Ex2.12
(defn mystery [ls]
  (if (empty? (rest (rest ls)))
    (cons (first ls) '())
    (cons (first ls) (mystery (rest ls)))))


(mystery '(1 2 3 4 5))
;;=> (1 2 3 4)
(mystery '(1 2 3 4))
;;=> (1 2 3)
(mystery '(1 2 3))
;;=> (1 2)
(mystery '(1 2))
;;=> (1)

(mystery '(1))

(mystery '())

;; mystery remove the last element so good name is remove-last


;;Ex2.13
(defn subst-1st [new old ls]
  (cond
    (empty? ls) '()
    (= (first ls) old) (cons new (rest ls))
    :else (cons (first ls) (subst-1st new old (rest ls)))))

(subst-1st 'dog 'cat '(my cat is clever))
(subst-1st 'b 'a '(c a b a c))
(subst-1st '(0) '(*) '((*) (1) (*) (2)))
(subst-1st 'two 'one '())


;;Ex2.14

(defn insert-right-1st [new old ls]
  (cond
    (empty? ls) '()
    (= (first ls) old) (cons old (cons new (rest ls)))
    :else (cons (first ls) (insert-right-1st new old (rest ls)))))

(insert-right-1st 'not 'does '(my dog does have fleas))

(defn insert-left-1st [new old ls]
  (cond
    (empty? ls) '()
    (= (first ls) old) (cons new ls)
    :else (cons (first ls)
                (insert-left-1st new old (rest ls)))))

(insert-left-1st 'hot 'dogs '(I eat dogs))
(insert-left-1st 'fun 'games '(some fun))
(insert-left-1st 'a 'b '(a b c a b c))
(insert-left-1st 'a 'b '())

;;Ex2.15
(defn list-of-first-items [ls]
  (if (empty? ls)
    '()
    (cons (first (first ls)) (list-of-first-items (rest ls)))))

(list-of-first-items '((a) (b c d) (e f)))
(list-of-first-items '((1 2 3)  (4 5 6)))
(list-of-first-items '((one)))
(list-of-first-items '())

;;Ex2.16
(defn replace [new-item ls]
  (if (empty? ls)
    '()
    (cons new-item (replace new-item (rest ls)))))

(replace 'no '(will you do me a favor))
(replace 'yes '(do you like ice cream))
(replace 'why '(not))
(replace 'maybe '())

;;Ex2.17
(defn remove-2nd [item ls]
  (cond
    (empty? ls) '()
    (= (first ls) item) (cons item (remove-1st item (rest ls)))
    :else (cons (first ls) (remove-2nd item (rest ls)))))

(remove-2nd 'cat '(my cat loves cat food))
(remove-2nd 'cat '(my cat loves food))
(remove-2nd 'cat '(my cat and your cat loves cat food))
(remove-2nd 'cat '())


;;Ex 2.18
(defn remove-last [item ls]
  (cond
    (empty? ls) '()
    (= (first ls) item) (if (member? item (rest ls))
                          (cons item (remove-last item (rest ls)))
                          (rest ls))
    :else (cons (first ls) (remove-last item (rest ls)))))


(remove-last 'a '(b a n a n a s))
(remove-last 'a '(b a n a n a))
(remove-last 'a '())
(remove-last 'a '(b c d e))
(remove-last 'a '(a b c d e))

(defn sandwich-1st [a b ls]
  (cond
    (empty? ls) '()
    (= (first ls) b)
       (if (= (first (rest ls)) b)
         (cons b (cons a (rest ls)))
         (cons b (sandwich-1st a b (rest ls))))
    :else (cons (first ls) (sandwich-1st a b (rest ls)))))

(sandwich-1st 'meat 'bread '(bread cheese bread bread))
(sandwich-1st 'meat 'bread '(bread  jam bread cheese bread))
(sandwich-1st 'meat 'bread '())

;;Ex2.20
(defn list-of-symbols? [ls]
  (cond
    (empty? ls) true
    (symbol? (first ls)) (list-of-symbols? (rest ls))
    :else false ))

(list-of-symbols? '(one two three four five))
(list-of-symbols? '(cat dog (hen pig) cow))
(list-of-symbols? '(a b 3 4 d))
(list-of-symbols? '())

(defn list-of-symbols? [ls]
  (if (empty? ls)
    true
    (if (symbol? (first ls))
      (list-of-symbols? (rest ls))
      false)))

(defn list-of-symbols? [ls]
  (or (empty? ls)
      (and (symbol? (first ls))
           (list-of-symbols? (rest ls)))))


;;Ex2.21

(defn all-same? [ls]
  (cond
    (empty? ls) true
    (empty? (rest ls)) true
    (= (first ls) (first (rest ls))) (all-same? (rest (rest ls)))
    :else false))


(all-same? '(a a a a a))
(all-same? '(a b a b a b))
(all-same? '((a b) (a b) (a b)))
(all-same? '(a))
(all-same? '())


(do
  (println "The remove-1st expression")
  (println "is applied to the list (1 2 3 4)")
  (println "to build a new list without the number 2.")
  (remove-1st 2 '(1 2 3 4)))


(do
 (+ 3 4)
 (- 5 11)
 (* 10 10))


(defn entering [test input cond-clause-number]
  (do
    (if test (println " Entering cond-clause-"
                      cond-clause-number "with ls =" input))
    test))

(defn leaving [result cond-clause-number]
  (do
    (println "Leaving cond-clause-"
             cond-clause-number "with result -" result)
    result))

(defn remove-1st-trace [item ls]
  (cond
    (entering (empty? ls) ls 1)
    (leaving '() 1)

    (entering (= (first ls) item) ls 2)
    (leaving (rest ls) 2)

    (entering 'else ls 3)
    (leaving (cons (first ls)
                   (remove-1st-trace item (rest ls))) 3)))

(remove-1st-trace 'c '(a b c d))
(remove-1st-trace 'e '(a b c d))


(defn swapper [x y ls]
  (cond
    (empty? ls) '()
    
    (= (first ls) x)
    (cons y (swapper x y (rest ls)))

    (= (first ls) y)
    (cons x (swapper x y (rest ls)))

    :else
    (cons (first ls) (swapper x y (rest ls)))))


(defn swap-tester [x y a]
  (cond
    (= a x) y
    (= a y) x
    :else a))

(defn swapper [x y ls]
  (cond
    (empty? ls) '()
    :else
    (cons (swap-tester x y (first ls))
          (swapper x y (rest ls)))))

(defn swapper [x y ls]
  (cond
    (empty? ls) '()
    :else (cons (cond
                  (= (first ls) x) y
                  (= (first ls) y) x
                  :else (first ls))
                (swapper x y (rest ls)))))

(swapper 'cat 'dog '(my cat eats dog food))
(swapper 'john 'mary '(john loves mary))
(swapper 'a 'b  '(c (a b) d))
(swapper 'a 'b '())
(swapper 'b 'd '(a b c d b))


;;Ex2.24
(defn describe [s]
  (cond
    (and (list? s) (empty? s)) (quote '())
    
    (number? s) s
        
    (symbol? s) (list 'quote s)
    
    (list? s) (list 'cons (describe (first s)) (describe (rest s)))
    
    :else s))


(describe '())
(describe 347)
;; => 347
(describe 'hello)
;; => (quote hello)
(describe '(a))
;; => (cons (quote a) (quote ()))
(describe '(a b))
;; => (cons (quote a) (cons (quote b) (quote ())))
(describe '(1 2 button my shoe))
;; => (cons 1 (cons 2 (cons (quote button) (cons (quote my) (cons (quote shoe) (quote ()))))))
(describe '(a (b c (d e) f g) h))
;; => (cons (quote a) (cons (cons (quote b) (cons (quote c) (cons (cons (quote d) (cons (quote e) (quote ()))) (cons (quote f) (cons (quote g) (quote ())))))) (cons (quote h) (quote ()))))


;;Ex2.25

(defn swapper-trace [x y ls]
  (cond
    (entering (empty? ls) ls 1)
    (leaving '() 1)
    
    (entering (= (first ls) x) ls 2)
    (leaving (cons y (swapper-trace x y (rest ls))) 2)

    (entering (= (first ls) y) ls 3)
    (leaving (cons x (swapper-trace x y (rest ls))) 3)

    (entering 'else ls 4)
    (leaving (cons (first ls) (swapper-trace x y (rest ls))) 4)))

(swapper-trace 'b 'd '(a b c d b))

;;Ex2.28

(defn tracing [message result]
  (do
    (println message result)
    result))

(defn test-tracing [test message input]
  (do
    (if test (tracing message input))
    test))


(defn remove-1st-trace [item ls]
  (cond
    (test-tracing (empty? ls) " Entering cond-clause-1 with ls = " ls)
    (tracing  "Leaving cond-clause-1 with result = " '())

    (test-tracing  (= (first ls) item) " Entering cond-clause-2 with ls = " ls)
    (tracing "Leaving cond-clause-2 with result = " (rest ls))

    (test-tracing 'else " Entering cond-clause-3 with ls = " ls)
    (tracing "Leaving cond-clause-1 with result = " (cons (first ls)
                   (remove-1st-trace item (rest ls))))))

(remove-1st-trace 'c '(a b c d))


(remove-1st-trace 'e '(a b c d))
