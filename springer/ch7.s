; - Program 7.1, pg. 196 -

(define map
  (lambda (proc ls)
    (if (null? ls)
        '()
        (cons (proc (car ls)) (map proc (cdr ls))))))

; - End Program -

; - Program 7.2, pg. 197 -

(define for-each
  (lambda (proc ls)
    (if (not (null? ls))
        (begin
          (proc (car ls))
          (for-each proc (cdr ls))))))

; - End Program -

; - Program 7.3, pg. 198 -

(define add
  (letrec ((list-add
             (lambda (ls)
               (if (null? ls)
                   0
                   (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

; - End Program -

; - Program 7.4, pg. 199 -

(define list (lambda args args))

; - End Program -

; - Program 7.5, pg. 199 -

(define writeln
  (lambda args
    (for-each display args)
    (newline)))

; - End Program -

; - Program 7.6, pg. 199 -

(define error
  (lambda args
    (display "Error:")
    (for-each (lambda (value) (display " ") (display value)) args)
    (newline)
    (reset)))

; - End Program -

; - Program 7.7, pg. 200 -

(define add
  (lambda args
    (if (null? args)
        0
        (+ (car args) (apply add (cdr args))))))

; - End Program -

; - Program 7.8, pg. 201 -

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x))))) 

; - End Program -

; - Program 7.9, pg. 203 -

(define plus
  (lambda (x y)
    (if (zero? y)
        x
        (add1 (plus x (sub1 y)))))) 

; - End Program -

; - Program 7.10, pg. 203 -

(define times
  (lambda (x y)
    (if (zero? y)
        0
        (plus x (times x (sub1 y)))))) 

; - End Program -

; - Program 7.11, pg. 203 -

(define exponent
  (lambda (x y)
    (if (zero? y)
        1
        (times x (exponent x (sub1 y))))))  

; - End Program -

; - Program 7.12, pg. 203 -

(define super
  (lambda (x y)
    (if (zero? y)
        1
        (exponent x (super x (sub1 y))))))  

; - End Program -

; - Program 7.13, pg. 204 -

(define superduper
  (lambda (x y)
    (if (zero? y)
        1
        (super x (superduper x (sub1 y)))))) 

; - End Program -

; - Program 7.14, pg. 204 -

(define super-order
  (lambda (n)
    (cond
      ((= n 1) plus)
      ((= n 2) times)
      (else (lambda (x y)
              (cond
                ((zero? y) 1)
                (else ((super-order (sub1 n))
                       x
                       ((super-order n) x (sub1 y))))))))))

; - End Program -

; - Program 7.15, pg. 205 -

(define ackermann
  (lambda (n)
    ((super-order n) n n))) 

; - End Program -

; - Exercise 7.5, pg. 206 -

(define iota
  (lambda (n)
    (letrec ((iota-helper
               (lambda (k acc)
                 (cond
                   ((zero? k) (cons 0 acc))
                   (else (iota-helper (sub1 k) (cons k acc)))))))
      (iota-helper (sub1 n) '()))))

(define mystery
  (lambda (len base)
    (letrec
      ((mystery-help
         (lambda (n s)
           (if (zero? n)
               (list s)
               (let ((h (lambda (x) 
                          (mystery-help (sub1 n) (cons x s)))))
                 (apply append (map h (iota base))))))))
      (mystery-help len '()))))

; - End Exercise -

; - Exercise 7.8, pg. 208 -

(define andmap
  (lambda (pred ls)
    (reduce (lambda (x y) (and x y)) (map pred ls))))

; - End Exercise -

; - Exercise 7.10, pg. 209 -

(define ormap
  (lambda (pred ls)
    (if (null? ls) 
        #f 
        (or (pred (car ls)) (ormap pred (cdr ls))))))

; - End Exercise -

; - Program 7.16, pg. 212 -

(define member?-c
  (lambda (item) 
    (letrec ((helper
               (lambda (ls)
                 (if (null? ls)
                     #f
                     (or (equal? (car ls) item)                   
                         (helper (cdr ls)))))))
      helper)))

; - End Program -

; - Program 7.17, pg. 213 -

(define apply-to-all
  (lambda (proc)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
               '()
               (cons (proc (car ls)) (helper (cdr ls)))))))
      helper)))

; - End Program -

; - Program 7.18, pg. 213 -

(define sum
  (letrec
    ((helper
       (lambda (ls)
         (if (null? ls)
             0
             (+ (car ls) (helper (cdr ls)))))))
    helper))

; - End Program -

; - Program 7.19, pg. 213 -

(define product
  (letrec
    ((helper
       (lambda (ls)
         (if (null? ls)
             1
             (* (car ls) (helper (cdr ls)))))))
    helper))

; - End Program -

; - Program 7.20, pg. 214 -

(define swapper-m
  (lambda (x y)
    (letrec
      ((helper
         (lambda (ls)
           (cond
             ((null? ls) '())
             ((equal? (car ls) x) (cons y (helper (cdr ls))))
             ((equal? (car ls) y) (cons x (helper (cdr ls))))
             (else (cons (car ls) (helper (cdr ls))))))))
      helper)))

; - End Program -

; - Exercise 7.14, pg. 215 -

(define round-5-places (round-n-places 5))

; - End Exercise -

; - Exercise 7.19, pg. 216 -

(define andmap-c
  (lambda (pred)
    (letrec
      ((and-help 
         (lambda (ls)
           (cond
             ((null? ls) #t)
             (else (and (pred (car ls)) (and-help (cdr ls))))))))
      and-help)))

(define all-positive? (andmap-c positive?))

(define ormap
  (lambda (pred ls)
    ((ormap-c pred) ls)))

(define some-positive? (ormap-c positive?))

; - End Exercise -

; - Exercise 7.20, pg. 217 -

(define divides-by
  (lambda (n)
    (lambda (k)
      (zero? (remainder k n)))))

; - End Exercise -

; - Program 7.23, pg. 221 -

(define flat-recur
  (lambda (seed list-proc)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
               seed
               (list-proc (car ls) (helper (cdr ls)))))))
      helper)))  

; - End Program -

; - Program 7.24, pg. 222 -

(define filter-in-c
  (lambda (pred)
    (flat-recur 
      '() 
      (lambda (x y)
        (if (pred x)
            (cons x y)
            y)))))  

; - End Program -

; - Program 7.25, pg. 225 -

(define filter-in-all-c
  (lambda (pred)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
               '()
               (let ((a (car ls))) 
                 (if (or (pair? a) (null? a))
                     (cons (helper a) (helper (cdr ls)))
                     (if (pred a)
                         (cons a (helper (cdr ls)))
                         (helper (cdr ls)))))))))
      helper))) 

; - End Program -

; - Program 7.26, pg. 225 -

(define filter-in-all
  (lambda (pred ls)
    ((filter-in-all-c pred) ls)))   

; - End Program -

; - Program 7.27, pg. 225 -

(define sum-all
  (letrec ((helper
             (lambda (ls)
               (if (null? ls)
                   0
                   (let ((a (car ls))) 
                     (if (or (pair? a) (null? a))
                         (+ (helper a) (helper (cdr ls)))
                         (+ a (helper (cdr ls)))))))))
    helper))  

; - End Program -

; - Program 7.28, pg. 227 -

(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
               seed
               (let ((c (car ls)))
                 (if (or (pair? c) (null? c))
                     (list-proc (helper c) (helper (cdr ls)))
                     (item-proc c (helper (cdr ls)))))))))
      helper)))

; - End Program -

