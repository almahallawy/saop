; -*- mode: clojure;-*-

;;run clojure REPL without a project:  M-x cider-jack-in
;; C-x C-e evalute last expr

;; Chapter 1 - Data and Operators

(println "Hello Clojure")

(def ten 10)

(quote ten)

(quote abc3)

'abc3


(def Robert 'Bob)
Robert

(cons 1 nil)
(conj nil 1)

(cons 1 '())
(conj '() 1)

(def ls1 (cons 1 nil))
ls1

(cons 2 ls1)
ls1

(def ls2 (cons 2 ls1))
ls2
ls1

(def c 'three)
(cons c ls2)

(def ls3 (cons c ls2))
ls3

(cons ls2 ls3)
ls3

(def ls4 (cons ls2 ls3))
ls4

(cons (cons 2 (cons 1 nil)) (cons 'three (cons 2 (cons 1 nil))))


;; car in Scheme
(first '(1 2 3 4))

(first ls4)

(first '(()))

;; cdr is Scheme
(rest '(1 2 3 4))
(rest ls4)

(first (rest '(a b c d)))

;;(cons 'a 'b) => fail


(def num 35.4)
(def twelve 'dozen)

(number? -45.67)
(number? num)
(number? twelve)
(number? (first '(15.3 -31.7)))
(number? (rest '(15.3 -31.7)))


(symbol? num)
(symbol? 'num)
(symbol? twelve)
(symbol? 'twelve)
(symbol? false)
(symbol? (first '(banana cream)))
(symbol? (rest '(banana cream)))
(symbol? 5)


(boolean? true)
(boolean? (number? 'a))
(boolean? (cons 'a '()))

(nil? nil)
(nil? '())
(nil? (rest '(cat)))
(nil? (first '((a b))))

(fn? cons)
(fn? +)
(fn? 'cons)
(fn? 100)

(ifn? cons)
(ifn? +)
(ifn? 'cons) ;;true??!!
(ifn? 100)


(= 3 (/ 6 2))
(= (/ 12 2) (* 2 3))
eq?(= (first '(-1 ten 543)) (/ -20 (* 4 5)))
(= (* 2 100) 20)


(def Garfield 'cat)
(= 'cat 'cat)
(= Garfield 'cat)
(= Garfield Garfield)
(= 'Garfield 'cat)
(= (first '(Garfield cat)) 'cat)
(= (first '(Garfield cat)) 'Garfield)


(def ls-a (cons 1 '(2 3)))
(def ls-b (cons 1 '(2 3)))
(def ls-c ls-a)
(= (cons 1 '(2 3)) (cons 1 '(2 3)))
(= ls-a (cons 1 '(2 3)))
(= ls-a ls-b)
(= ls-c ls-a)


(identical? '(a b c) (cons 'a '(b c)))
