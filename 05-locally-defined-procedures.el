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
