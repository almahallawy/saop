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
