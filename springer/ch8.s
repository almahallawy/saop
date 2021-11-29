; - Program 8.1, pg. 232 -

(define both 
  (lambda (pred)
    (lambda (arg1 arg2)
      (and (pred arg1) (pred arg2)))))

; - End Program -

; - Program 8.2, pg. 232 -

(define neither 
  (lambda (pred)
    (lambda (arg1 arg2)
      (not (or (pred arg1) (pred arg2))))))

; - End Program -

; - Program 8.3, pg. 233 -

(define at-least-one 
  (lambda (pred)
    (lambda (arg1 arg2)
      (or (pred arg1) (pred arg2)))))

; - End Program -

; - Program 8.4, pg. 237 -

(define make-set
  (lambda args
    (letrec 
      ((list-make-set
         (lambda (args-list)
           (if (null? args-list) 
               the-empty-set
               (adjoin 
                 (car args-list) 
                 (list-make-set (cdr args-list)))))))
      (list-make-set args))))

; - End Program -

; - Program 8.5, pg. 238 -

(define none
  (lambda (pred)
    (letrec 
      ((test
         (lambda (s)
           (or (empty-set? s)
               (let ((elem (pick s)))
                 (and (not (pred elem))
                      (test ((residue elem) s))))))))
       test)))

; - End Program -

; - Program 8.6, pg. 238 -

(define there-exists
  (lambda (pred)
    (compose not (none pred))))

; - End Program -

; - Program 8.7, pg. 239 -

(define for-all
  (lambda (pred)
    (none (compose not pred))))

; - End Program -

; - Program 8.8, pg. 240 -

(define set-equal
  (lambda (obj1)
    (lambda (obj2)
      (or (and ((neither set?) obj1 obj2)
               (equal? obj1 obj2))
          (and ((both set?) obj1 obj2)
               ((subset obj1) obj2)
               ((subset obj2) obj1))))))

; - End Program -

; - Program 8.9, pg. 241 -

(define element (compose there-exists set-equal))

; - End Program -

; - Program 8.10, pg. 241 -

(define contains
  (lambda (set)
    (lambda (e)
      ((element e) set))))

; - End Program -

; - Program 8.11, pg. 242 -

(define superset
  (lambda (s1)
    (lambda (s2)
      ((for-all (contains s1)) s2))))

; - End Program -

; - Program 8.12, pg. 242 -

(define subset
  (lambda (s1)
    (lambda (s2)
      ((superset s2) s1))))

; - End Program -

; - Program 8.13, pg. 242 -

(define cardinal
  (lambda (s)
    (if (empty-set? s) 
        0
        (let ((elem (pick s)))
          (add1 (cardinal ((residue elem) s)))))))

; - End Program -

; - Program 8.14, pg. 243 -

(define intersection
  (lambda (s1 s2)
    (letrec 
      ((helper
         (lambda (s1)
           (if (empty-set? s1)
               the-empty-set
               (let ((elem (pick s1)))
                 (if ((contains s2) elem) 
                     (adjoin elem (helper ((residue elem) s1)))
                     (helper ((residue elem) s1))))))))
      (helper s1))))

; - End Program -

; - Program 8.15, pg. 244 -

(define union
  (lambda (s1 s2)
    (letrec 
      ((helper
         (lambda (s1)
           (if (empty-set? s1) 
               s2
               (let ((elem (pick s1)))
                 (if (not ((contains s2) elem))
                     (adjoin elem (helper ((residue elem) s1)))
                     (helper ((residue elem) s1))))))))
      (helper s1))))

; - End Program -

; - Program 8.16, pg. 244 -

(define difference
  (lambda (s1 s2)   
    (letrec
      ((helper
         (lambda (s1)
           (if (empty-set? s1) 
               the-empty-set
               (let ((elem (pick s1)))
                 (if (not ((contains s2) elem))
                     (adjoin elem (helper ((residue elem) s1)))
                     (helper ((residue elem) s1))))))))
      (helper s1))))

; - End Program -

; - Program 8.17, pg. 245 -

(define set-builder
  (lambda (pred base-set)
    (letrec 
      ((helper
         (lambda (s)
           (if (empty-set? s) 
               base-set
               (let ((elem (pick s))) 
                 (if (pred elem) 
                     (adjoin elem (helper ((residue elem) s)))
                     (helper ((residue elem) s))))))))
       helper)))

; - End Program -

; - Program 8.19, pg. 246 -

(define family-union
  (lambda (s)
    (if (empty-set? s)
        the-empty-set
        (let ((elem (pick s)))
          (union elem (family-union ((residue elem) s)))))))

; - End Program -

; - Program 8.20, pg. 246 -

(define family-intersection
  (lambda (s)
    (if (empty-set? s)
        the-empty-set
        (letrec
          ((fam-int
             (lambda (s)
               (let ((elem (pick s)))
                 (let ((rest ((residue elem) s)))
                   (if (empty-set? rest)
                        elem
                        (intersection elem (fam-int rest))))))))
          (fam-int s)))))

; - End Program -

; - Program 8.21, pg. 247 -

(define set-map
  (lambda (proc s)
    (if (empty-set? s)
        the-empty-set
        (let ((elem (pick s)))
          (adjoin (proc elem) 
                  (set-map proc ((residue elem) s)))))))

; - End Program -

; - Program 8.22, pg. 247 -

(define list->set
  (lambda (ls)
    (apply make-set ls)))

; - End Program -

; - Program 8.23, pg. 248 -

(define set->list
  (lambda (s)
    (if (empty-set? s)
        '()
        (let ((elem (pick s)))
          (cons elem (set->list ((residue elem) s)))))))

; - End Program -

; - Exercise 8.5, pg. 248 -

(define for-one
  (lambda (pred found-proc not-found-proc)
    (letrec ((test 
               (lambda (s)
                 (if (empty-set? s) 
                     (not-found-proc)
                     (let ((v (pick s)))
                       (if (pred v) 
                           (found-proc v)
                           (test ((residue v) s))))))))
      test)))

; - End Exercise -

; - Exercise 8.6, pg. 249 -

(define superset (compose for-all contains))

; - End Exercise -

; - Program 8.24, pg. 250 -

(define the-empty-set (cons set-tag '()))

(define empty-set?
  (lambda (s)
    (eq? s the-empty-set)))

(define set?
  (lambda (arg)
    (and (pair? arg) (eq? (car arg) set-tag))))

(define pick
  (lambda (s)
    (let ((ls (cdr s)))
      (if (null? ls)
          (error "pick: The set is empty.")
          (list-ref ls (random (length ls)))))))

; - End Program -

; - Program 8.25, pg. 251 -

(define adjoin
  (lambda (elem s)
    (cons set-tag (cons elem (cdr s)))))
    
(define residue
  (lambda (elem)
    (lambda (s)
      (let ((ls (remove elem (cdr s))))
        (cond
          ((null? ls) the-empty-set)
          (else (cons set-tag ls)))))))

; - End Program -

; - Program 8.26, pg. 252 -

(define adjoin
  (lambda (elem s)
    (cond
      ((member? elem (cdr s)) s)
      (else (cons set-tag (cons elem (cdr s)))))))

(define residue
  (lambda (elem)
    (lambda (s)
      (let ((ls (remove-1st elem (cdr s))))
        (cond
          ((null? ls) the-empty-set)
          (else (cons set-tag ls)))))))

; - End Program -

; - Exercise 8.7, pg. 253 -

(define pick
  (lambda (s)
    (car (cdr s))))

; - End Exercise -

; - Program 8.27, pg. 256 -

(define make-op
  (lambda (x y)
    (make-set (make-set x) (make-set x y))))

(define op?
  (lambda (s)
    (and (set? s)
         ((for-all set?) s)
         (= (cardinal (family-intersection s)) 1)
         (or (= (cardinal s) 1)
             ((both (lambda (x) (= (cardinal x) 2)))
              s
              (family-union s))))))

(define op-1st
  (lambda (pr)
    (pick (family-intersection pr))))

(define op-2nd
  (lambda (pr)
    (let ((fam-int (family-intersection pr)))
      (let ((diff (difference (family-union pr) fam-int)))
        (pick (if (empty-set? diff) fam-int diff))))))

; - End Program -

; - Program 8.28, pg. 257 -

(define make-op 
  (lambda (x y)
    (list x y)))

(define op?
  (lambda (arg)
    (and (pair? arg) (pair? (cdr arg)) (null? (cddr arg)))))

(define op-1st 
  (lambda (pr)
    (car pr)))

(define op-2nd
  (lambda (pr)
    (cadr pr)))

; - End Program -

; - Program 8.29, pg. 257 -

(define make-op 
  (lambda (x y)
    (cons x y)))

(define op? 
  (lambda (arg)
    (pair? arg)))

(define op-1st 
  (lambda (pr)
    (car pr)))

(define op-2nd 
  (lambda (pr)
    (cdr pr)))

; - End Program -

; - Program 8.30, pg. 258 -

(define cartesian-product
  (lambda (s1 s2)
    (if (empty-set? s1) 
        the-empty-set
        (let ((elem (pick s1)))
          (union (set-map (lambda (x) (make-op elem x)) s2)
                 (cartesian-product ((residue elem) s1) s2))))))

; - End Program -

; - Program 8.31, pg. 259 -

(define domain
  (lambda (rel)
    (set-map op-1st rel)))

(define range
  (lambda (rel)
    (set-map op-2nd rel)))

; - End Program -

; - Program 8.32, pg. 260 -

(define subrelation/1st
  (lambda (rel)
    (lambda (arg)
      ((set-builder 
         (lambda (x) ((set-equal (op-1st x)) arg)) 
         the-empty-set)
       rel))))

; - End Program -

; - Program 8.33, pg. 260 -

(define function?
  (lambda (rel)
    (or (empty-set? rel)
        (let ((subrel ((subrelation/1st rel) (op-1st (pick rel)))))
          (and (= (cardinal (set-map op-2nd subrel)) 1)
               (function? (difference rel subrel)))))))

; - End Program -

; - Program 8.34, pg. 261 -

(define value
  (lambda (fun)
    (lambda (arg)
      (op-2nd (pick ((subrelation/1st fun) arg))))))

; - End Program -


