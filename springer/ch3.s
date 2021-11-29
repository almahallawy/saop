; - Program 3.1, pg. 75 -

(define add1
  (lambda (n)
    (+ n 1)))

; - End Program -

; - Program 3.2, pg. 75 -

(define sub1
  (lambda (n)
    (- n 1)))

; - End Program -

; - Program 3.4, pg. 77 -

(define harmonic-sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ (/ 1 n) (harmonic-sum (sub1 n)))))))

; - End Program -

; - Program 3.5, pg. 78 -

(define list-of-zeros
  (lambda (n)
    (cond
      ((zero? n) '())
      (else (cons 0 (list-of-zeros (sub1 n)))))))

; - End Program -

; - Program 3.6, pg. 79 -

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (add1 (length (cdr ls))))))

; - End Program -

; - Program 3.7, pg. 81 -

(define list-ref
  (lambda (ls n)
    (cond
      ((null? ls)
       (error "list-ref: Index" n "out of range for list" ls))
      ((zero? n) (car ls))
      (else (list-ref (cdr ls) (sub1 n))))))

; - End Program -

; - Program 3.8, pg. 85 -

(define rzero?
  (lambda (rtl)
    (zero? (numr rtl))))

; - End Program -

; - Program 3.9, pg. 86 -

(define r+
  (lambda (x y)
    (make-ratl 
      (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
      (* (denr x) (denr y))))) 

; - End Program -

; - Program 3.10, pg. 86 -

(define r*
  (lambda (x y)
    (make-ratl 
      (* (numr x) (numr y)) 
      (* (denr x) (denr y)))))

; - End Program -

; - Program 3.11, pg. 86 -

(define r-
  (lambda (x y)
    (make-ratl 
      (- (* (numr x) (denr y)) (* (numr y) (denr x)))
      (* (denr x) (denr y)))))

; - End Program -

; - Program 3.12, pg. 87 -

(define rinvert 
  (lambda (rtl)
    (if (rzero? rtl)
        (error "rinvert: Cannot invert " rtl)
        (make-ratl (denr rtl) (numr rtl)))))

; - End Program -

; - Program 3.13, pg. 87 -

(define r/
  (lambda (x y)
    (r* x (rinvert y))))

; - End Program -

; - Program 3.14, pg. 87 -

(define r=
  (lambda (x y)
    (= (* (numr x) (denr y)) (* (numr y) (denr x)))))

; - End Program -

; - Program 3.15, pg. 87 -

(define rpositive?
  (lambda (rtl)
    (or (and (positive? (numr rtl)) (positive? (denr rtl)))
        (and (negative? (numr rtl)) (negative? (denr rtl))))))

; - End Program -

; - Program 3.16, pg. 88 -

(define r>
  (lambda (x y)
    (rpositive? (r- x y))))

; - End Program -

; - Program 3.17, pg. 88 -

(define max                    
  (lambda (x y)
    (if (> x y)
        x
        y))) 

; - End Program -

; - Program 3.18, pg. 88 -

(define rmax
  (lambda (x y)
    (if (r> x y)
        x
        y))) 

; - End Program -

; - Program 3.19, pg. 89 -

(define extreme-value
  (lambda (pred x y)
    (if (pred x y)
        x 
        y))) 

; - End Program -

; - Program 3.20, pg. 90 -

(define rprint
  (lambda (rtl)
    (writeln (numr rtl) "/" (denr rtl)))) 

; - End Program -

; - Program 3.21, pg. 91 -

(define numr
  (lambda (rtl)
    (car rtl)))

(define denr
  (lambda (rtl)
    (cadr rtl)))

(define make-ratl
  (lambda (int1 int2)
    (if (zero? int2)
        (error "make-ratl: The denominator cannot be zero.")
        (list int1 int2)))) 

; - End Program -

; - Exercise 3.15, pg. 92 -

(define rpositive?
  (lambda (rtl)
    (same-sign? (numr rtl) (denr rtl))))

; - End Exercise -
