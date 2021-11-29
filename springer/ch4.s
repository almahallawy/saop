; - Program 4.1, pg. 96 -

(define append
  (lambda (ls1 ls2)       
    (if (null? ls1) 
        ls2
        (cons (car ls1) (append (cdr ls1) ls2)))))

; - End Program -
 
; - Program 4.2, pg. 97 -

(define reverse
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse (cdr ls)) (list (car ls))))))

; - End Program -

; - Program 4.3, pg. 98 -

(define merge
  (lambda (sorted-ntpl1 sorted-ntpl2)
    (cond 
      ((null? sorted-ntpl1) sorted-ntpl2)
      ((null? sorted-ntpl2) sorted-ntpl1)
      ((< (car sorted-ntpl1) (car sorted-ntpl2))
       (cons (car sorted-ntpl1) 
             (merge (cdr sorted-ntpl1) sorted-ntpl2)))
      (else (cons (car sorted-ntpl2) 
                  (merge sorted-ntpl1 (cdr sorted-ntpl2)))))))

; - End Program -
 
; - Program 4.4, pg. 98 -

(define even? 
  (lambda (int)
    (if (zero? int)
        #t
        (odd? (sub1 int)))))

; - End Program -

; - Program 4.5, pg. 99 -

(define odd?
  (lambda (int)
    (if (zero? int)
        #f
        (even? (sub1 int)))))

; - End Program -
 
; - Program 4.6, pg. 100 -

(define remove
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (remove item (cdr ls)))
      (else (cons (car ls) (remove item (cdr ls)))))))

; - End Program -

; - Program 4.7, pg. 102 -

(define count-all
  (lambda (ls)
    (cond
      ((null? ls) 0)
      ((not (pair? (car ls))) (add1 (count-all (cdr ls))))
      (else (+ (count-all (car ls)) (count-all (cdr ls)))))))

; - End Program -

; - Program 4.8, pg. 105 -

(define remove-all
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (remove-all item (cdr ls)))
      ((pair? (car ls)) 
       (cons (remove-all item (car ls)) (remove-all item (cdr ls))))
      (else (cons (car ls) (remove-all item (cdr ls)))))))

; - End Program -

; - Program 4.9, pg. 105 -

(define remq-all
  (lambda (symbl ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls))
       (cons (remq-all symbl (car ls)) (remq-all symbl (cdr ls))))
      ((eq? (car ls) symbl) (remq-all symbl (cdr ls)))
      (else (cons (car ls) (remq-all symbl (cdr ls)))))))

; - End Program -

; - Program 4.10, pg. 107 -

(define reverse-all
  (lambda (ls)
    (if (null? ls) 
        '()
        (append (reverse-all (cdr ls))
                (list (if (pair? (car ls))
                          (reverse-all (car ls))
                          (car ls)))))))

; - End Program -

; - Program 4.13, pg. 110 -

(define depth
  (lambda (item)
    (if (not (pair? item)) 
        0
        (max (add1 (depth (car item))) (depth (cdr item))))))

; - End Program -

; - Program 4.14, pg. 111 -

(define flatten
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls))
       (append (flatten (car ls)) (flatten (cdr ls))))
      (else (cons (car ls) (flatten (cdr ls)))))))

; - End Program -

; - Program 4.15, pg. 113 -

(define remove-left-most
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      ((not (pair? (car ls)))
       (cons (car ls) (remove-left-most item (cdr ls))))
      ((member-all? item (car ls))
       (cons (remove-left-most item (car ls)) (cdr ls)))
      (else (cons (car ls) (remove-left-most item (cdr ls)))))))

; - End Program -

; - Program 4.16, pg. 113 -

(define member-all?
  (lambda (item ls)
    (if (null? ls) 
        #f
        (or (equal? (car ls) item)
            (and (not (pair? (car ls)))
                 (member-all? item (cdr ls)))
            (and (pair? (car ls))
                 (or (member-all? item (car ls))
                     (member-all? item (cdr ls))))))))

; - End Program -

; - Program 4.17, pg. 114 -

(define remove-left-most
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      ((and (pair? (car ls)) (member-all? item (car ls)))
       (cons (remove-left-most item (car ls)) (cdr ls)))
      (else (cons (car ls) (remove-left-most item (cdr ls)))))))

; - End Program -

; - Program 4.18, pg. 116 -

(define fact
  (lambda (int)
    (if (zero? int)
        1
        (* int (fact (sub1 int))))))

; - End Program -

; - Program 4.19, pg. 118 -

(define fact-it
  (lambda (int acc)
    (if (zero? int)
        acc
        (fact-it (sub1 int) (* acc int)))))

; - End Program -

; - Program 4.20, pg. 121 -

(define fib
  (lambda (int)
    (if (< int 2) 
        int
        (+ (fib (- int 1)) (fib (- int 2))))))

; - End Program -

; - Program 4.24, pg. 124 -

(define fib-it
  (lambda (int acc1 acc2)
    (if (= int 1)
        acc2
        (fib-it (sub1 int) acc2 (+ acc1 acc2)))))

; - End Program -

; - Program 4.25, pg. 127 -

(define reverse-it
  (lambda (ls acc)
    (if (null? ls)
        acc
        (reverse-it (cdr ls) (cons (car ls) acc)))))

; - End Program -
