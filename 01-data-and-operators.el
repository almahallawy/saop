; -*- mode: Lisp;-*-

;; Chapter 1 - Data and Operators


(setq num 35.4)
(setq twelve 'dozen)


(numberp -45.67)
(numberp '3)
(numberp num)
(numberp twelve)
(numberp 'twelve)
(numberp (+ 2 3))
(numberp t)
(numberp (car '(15.3 -31.7)))
(numberp (cdr '(15.3 -31.7)))


(symbolp 15)
(symbolp num)
(symbolp 'num)
(symbolp twelve)
(symbolp 'twelve)
(symbolp nil)
(symbolp (car '(banana cream)))
(symbolp (cdr '(banana cream)))


(booleanp t)
(booleanp nil)
(booleanp (numberp 'a))
(booleanp (cons 'a '()))



;;https://www.gnu.org/software/emacs/manual/html_node/elisp/List_002drelated-Predicates.html
 ;; -- Function: consp object
 ;;     This function returns ‘t’ if OBJECT is a cons cell, ‘nil’
 ;;     otherwise.  ‘nil’ is not a cons cell, although it _is_ a list.

 ;; -- Function: atom object
 ;;     This function returns ‘t’ if OBJECT is an atom, ‘nil’ otherwise.
 ;;     All objects except cons cells are atoms.  The symbol ‘nil’ is an
 ;;     atom and is also a list; it is the only Lisp object that is both.

 ;;          (atom OBJECT) ≡ (not (consp OBJECT))

 ;; -- Function: listp object
 ;;     This function returns ‘t’ if OBJECT is a cons cell or ‘nil’.
 ;;     Otherwise, it returns ‘nil’.

 ;;          (listp '(1))
 ;;               ⇒ t
 ;;          (listp '())
 ;;               ⇒ t

;; elisp consp = pair? in scheme
(consp '(Ann Ben Carl))
(listp '(Ann Ben Carl))
(consp '(1))
(listp '(1))
(consp '()) ;==> nil
(listp '()) ;==> t
(consp '(()))
(listp '(()))
(consp '(a (b c) d))
(listp '(a (b c) d))
(consp (cons 'a '()))
(listp (cons 'a '()))
(consp (cons 3 4))
(listp (cons 3 4))
(consp 'pair)
(listp 'pair)


(null nil)
(null (cdr '(cat)))
(null (car '((a b))))

(functionp 'cons)
(functionp '+)
(functionp 'cons)
(functionp 100)
