; - Program 5.3, pg. 137 -

(define remove-leftmost
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      ((pair? (car ls))
       (let ((rem-list (remove-leftmost item (car ls))))
         (cons rem-list (cond
                          ((equal? (car ls) rem-list)
                           (remove-leftmost item (cdr ls)))
                          (else (cdr ls))))))
      (else (cons (car ls) (remove-leftmost item (cdr ls)))))))

; - End Program -

; - Program 5.4, pg. 139 -

(define fact
  (lambda (n)
    (letrec ((fact-it 
               (lambda (k acc)
                 (if (zero? k)
                     acc
                     (fact-it (sub1 k) (* k acc))))))
      (fact-it n 1))))  

; - End Program -

; - Program 5.5, pg. 139 -

(define swapper
  (lambda (x y ls)
    (letrec
      ((swap
         (lambda (ls*)
           (cond 
             ((null? ls*) '())
             ((equal? (car ls*) x) (cons y (swap (cdr ls*))))
             ((equal? (car ls*) y) (cons x (swap (cdr ls*))))
             (else (cons (car ls*) (swap (cdr ls*))))))))
      (swap ls))))

; - End Program -

; - Exercise 5.5, pg. 141 -

(define mystery
  (lambda (n)
    (letrec
      ((mystery-helper
         (lambda (n s)
           (cond
             ((zero? n) (list s))
             (else
               (append
                 (mystery-helper (sub1 n) (cons 0 s))
                 (mystery-helper (sub1 n) (cons 1 s))))))))
      (mystery-helper n '()))))

; - End Exercise -

; - Program 5.6, pg. 144 -

(define zero-poly?
  (lambda (poly)
    (and (zero? (degree poly)) (zero? (leading-coef poly)))))

; - End Program -

; - Program 5.7, pg. 145 -

(define make-term
  (lambda (deg coef)
    (poly-cons deg coef the-zero-poly))) 

; - End Program -

; - Program 5.8, pg. 145 -

(define leading-term
  (lambda (poly)
    (make-term (degree poly) (leading-coef poly))))

; - End Program -

; - Program 5.9, pg. 146 -

(define p+
  (lambda (poly1 poly2)
    (cond
      ((zero-poly? poly1) poly2)
      ((zero-poly? poly2) poly1)
      (else (let ((n1 (degree poly1))
                  (n2 (degree poly2))
                  (a1 (leading-coef poly1))
                  (a2 (leading-coef poly2))
                  (rest1 (rest-of-poly poly1))
                  (rest2 (rest-of-poly poly2)))
              (cond 
                ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
                ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
                (else 
                 (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))) 

; - End Program -

; - Program 5.10, pg. 148 -

(define p* 
  (letrec 
    ((t* (lambda (trm poly)
           (if (zero-poly? poly)
               the-zero-poly
               (poly-cons 
                 (+ (degree trm) (degree poly))
                 (* (leading-coef trm) (leading-coef poly))
                 (t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
        ((p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                          the-zero-poly
                          (p+ (t* (leading-term p1) poly2)
                              (p*-helper (rest-of-poly p1)))))))
        (p*-helper poly1)))))

; - End Program -

; - Program 5.11, pg. 148 -

(define negative-poly
  (lambda (poly) 
    (let ((poly-negative-one (make-term 0 -1)))
      (p* poly-negative-one poly))))

; - End Program -

; - Program 5.12, pg. 148 -

(define p-
  (lambda (poly1 poly2)
    (p+ poly1 (negative-poly poly2))))  

; - End Program -

; - Program 5.13, pg. 150 -

(define poly-value
  (lambda (poly num)
    (letrec 
      ((pvalue (lambda (p)
                 (let ((n (degree p)))
                   (if (zero? n) 
                       (leading-coef p)
                       (let ((rest (rest-of-poly p)))
                         (if (< (degree rest) (sub1 n))
                             (pvalue (poly-cons
                                       (sub1 n)
                                       (* num (leading-coef p))
                                       rest))
                             (pvalue (poly-cons 
                                       (sub1 n)
                                       (+ (* num (leading-coef p))
                                          (leading-coef rest))
                                       (rest-of-poly rest))))))))))
      (pvalue poly))))

; - End Program -

; - Program 5.14, pg. 151 -

(define the-zero-poly '(0))

(define degree 
  (lambda (poly)
    (sub1 (length poly))))

(define leading-coef
  (lambda (poly)
    (car poly)))

(define rest-of-poly
  (lambda (poly)
    (cond
      ((zero? (degree poly)) the-zero-poly)
      ((zero? (leading-coef (cdr poly)))
       (rest-of-poly (cdr poly)))
      (else (cdr poly)))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond
        ((and (zero? deg) (equal? poly the-zero-poly)) (list coef))
        ((< deg-p deg)
         (if (zero? coef)
             poly
             (cons coef 
                (append (list-of-zeros (sub1 (- deg deg-p))) 
                        poly))))
        (else (error "poly-cons: Degree too high in" poly))))))

; - End Program -

; - Program 5.15, pg. 153 -

(define the-zero-poly '((0 0)))

(define degree
  (lambda (poly)
    (caar poly)))
    
(define leading-coef
  (lambda (poly)
    (cadar poly)))

(define rest-of-poly 
  (lambda (poly)
    (if (null? (cdr poly))
        the-zero-poly
        (cdr poly))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond 
        ((and (zero? deg) (equal? poly the-zero-poly))
         (list (list deg coef)))
        ((< deg-p deg)
         (if (zero? coef) 
             poly
             (cons (list deg coef) poly)))
        (else (error "poly-cons: degree too high in" poly))))))

; - End Program -

; - Program 5.16, pg. 157 -

(define digits->poly
  (lambda (digit-list)
    (if (null? digit-list)
        (error "digits->poly: Not defined for" digit-list)
        (letrec
          ((make-poly
             (lambda (deg ls)
               (if (null? ls)
                   the-zero-poly
                   (poly-cons deg (car ls)
                     (make-poly (sub1 deg) (cdr ls)))))))
          (make-poly (sub1 (length digit-list)) digit-list)))))

; - End Program -

; - Program 5.17, pg. 157 -

(define binary->decimal
  (lambda (digit-list)
    (poly-value (digits->poly digit-list) 2)))  

; - End Program -

; - Program 5.18, pg. 158 -

(define poly->digits
  (lambda (poly)
    (letrec
      ((convert
         (lambda (p deg)
           (cond
             ((zero? deg) (list (leading-coef p)))
             ((= (degree p) deg)
              (cons (leading-coef p)
                    (convert (rest-of-poly p) (sub1 deg))))
             (else
              (cons 0 (convert p (sub1 deg))))))))
      (convert poly (degree poly)))))

; - End Program -

; - Program 5.20, pg. 160 -

(define decimal->binary
  (lambda (num)
    (letrec
      ((dec->bin
         (lambda (n deg)
           (if (zero? n)
               the-zero-poly
               (p+ (make-term deg (remainder n 2))
                   (dec->bin (quotient n 2) (add1 deg)))))))
      (poly->digits (dec->bin num 0)))))

; - End Program -
