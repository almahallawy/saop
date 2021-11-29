; - Program 14.1, pg. 453 -

(macro freeze
  (lambda (code)
    (cons 'lambda (cons '() (list (2nd code))))))

; - End Program -

; - Program 14.2, pg. 356 -

(extend-syntax (freeze) 
  ((freeze expr1 expr2 ...) (lambda () expr1 expr2 ...)))

; - End Program -

; - Program 14.3, pg. 456 -

(define thaw
  (lambda (thunk)
    (thunk)))  

; - End Program -

; - Program 14.4, pg. 457 -

(define make-promise
  (lambda (thunk)
    (let ((already-run? #f)
          (result "any value"))
      (lambda ()
        (if (not already-run?)
            (begin
              (set! result (thaw thunk))
              (set! already-run? #t)))
        result))))

; - End Program -

; - Program 14.5, pg. 458 -

(extend-syntax (delay)
  ((delay expr1 expr2 ...) (make-promise (freeze expr1 expr2 ...))))

; - End Program -

; - Program 14.6, pg. 458 -

(define delay-transformer
  (lambda (code)
    (list 'make-promise (cons 'freeze (cdr code)))))

(macro delay delay-transformer)

; - End Program -

; - Program 14.7, pg. 459 -

(extend-syntax (let)
  ((let ((var val) ...) expr1 expr2 ...)
   ((lambda (var ...) expr1 expr2 ...) val ...)))

; - End Program -

; - Program 14.8, pg. 460 -

(define let-transformer
  (lambda (code)
    (cons (make-lambda-expression
            (make-list-of-parameters code)
            (make-list-of-body-items code))
          (make-list-of-operands code))))

(macro let let-transformer)

; - End Program -

; - Program 14.9, pg. 461 -

(extend-syntax (letrec)
  ((letrec ((var val) ...) expr1 expr2 ...)
   (let ((var "any") ...)
     (begin (set! var val) ...)
     expr1 expr2 ...)))

; - End Program -

; - Program 14.10, pg. 462 -

(macro letrec
  (lambda (code)
    (cons 'let
      (cons (map (lambda (x) (list (1st x) "any")) (2nd code))
        (cons (cons 'begin
                (map (lambda (x) (cons 'set! x)) (2nd code)))
          (cddr code))))))                

; - End Program -

; - Program 14.11, pg. 463 -

(define cycle-proc
  (lambda (th)
    (letrec ((loop (lambda ()
                     (th)
                     (loop))))
      (loop))))

; - End Program -

; - Program 14.12, pg. 464 -

(define or-proc
  (lambda (th-list)
    (cond
      ((null? th-list) #f)
      ((null? (cdr th-list)) (thaw (car th-list)))
      (else (let ((v (thaw (car th-list))))
              (if v v (or-proc (cdr th-list))))))))

; - End Program -

; - Program 14.13, pg. 465 -

(define or-transformer
  (lambda (expr)
    (list 'or-proc
          (cons 'list
                (map (lambda (e) (list 'freeze e))
                     (cdr expr))))))

(macro or or-transformer)

; - End Program -

; - Program 14.14, pg. 465 -

(extend-syntax (or)
  ((or e ...) (or-proc (list (freeze e) ...))))

; - End Program -

; - Exercise 14.3, pg. 466 -

(extend-syntax (or)
  ((or) #f)
  ((or e) e)
  ((or e1 e2 ...) (let ((val e1) (th (freeze (or e2 ...))))
                    (if val val (th)))))

; - End Exercise -

; - Exercise 14.12, pg. 470 -

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v))
          (sum 0))
      (for i 0 (add1 i) (= i n) (set! sum (+ (vector-ref v i) sum)))
      sum)))

; - End Exercise -

; - Exercise 14.16, pg. 472 -

(define member-trace
  (lambda (item ls)
    (cond
      ((null? ls) (writeln "no") #f)
      ((equal? (car ls) item) (writeln "yes") #t)
      (else (writeln "maybe") (member-trace item (cdr ls))))))

(define factorial
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (factorial (sub1 n)))))))

; - End Exercise -
