; - Exercise 2.3, pg. 39 -

(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

; - End Exercise -

; - Program 2.1, pg. 44 -

(define singleton-list?
  (lambda (ls)
    (and (pair? ls) (null? (cdr ls)))))

; - End Program -

; - Program 2.2, pg. 47 -

(define last-item
  (lambda (ls)
    (cond
      ((null? (cdr ls)) (car ls))
      (else (last-item (cdr ls))))))

; - End Program -

; - Program 2.3, pg. 50 -

(define member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      (else (or (equal? (car ls) item) 
                (member? item (cdr ls)))))))

; - End Program -

; - Program 2.4, pg. 52 -

(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      (else (cons (car ls) (remove-1st item (cdr ls)))))))

; - End Program -

; - Program 2.5, pg. 62 -

(define remove-1st-trace
  (lambda (item ls)
    (cond
      ((entering (null? ls) ls 1)
       (leaving '() 1))
      ((entering (equal? (car ls) item) ls 2)
       (leaving (cdr ls) 2))
      ((entering 'else ls 3)
       (leaving
         (cons (car ls) (remove-1st-trace item (cdr ls)))
         3)))))

; - End Program -

; - Program 2.6, pg. 62 -

(define entering
  (lambda (test input cond-clause-number)
    (begin
      (if test (writeln "   Entering cond-clause-"
                 cond-clause-number " with ls = " input))
      test)))

; - End Program -

; - Program 2.7, pg. 62 -

(define leaving
  (lambda (result cond-clause-number)
    (begin
      (writeln "Leaving cond-clause-" 
        cond-clause-number " with result = " result)
      result)))

; - End Program -

; - Program 2.8, pg. 66 -

(define swapper 
  (lambda (x y ls) 
    (cond
      ((null? ls) '())
      ((equal? (car ls) x) 
       (cons y (swapper x y (cdr ls))))
      ((equal? (car ls) y) 
       (cons x (swapper x y (cdr ls))))
      (else 
       (cons (car ls) (swapper x y (cdr ls)))))))      

; - End Program -

; - Exercise 2.24, pg. 70 -

(define describe
  (lambda (s)
    (cond
      ((null? s) (quote '()))
      ((number? s) s)
      ((symbol? s) (list 'quote s))
      ((pair? s) (list 'cons (describe (car s)) (describe (cdr s))))
      (else s))))

; - End Exercise -
