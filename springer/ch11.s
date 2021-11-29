; - Program 11.1, pg. 344 -

(define stk '())

(define empty? 
  (lambda () 
    (null? stk)))

(define top 
  (lambda () 
    (if (empty?)
        (error "top: The stack is empty.")
        (car stk))))

(define print-stack 
  (lambda () 
    (display "TOP: ")
    (for-each (lambda (x) (display x) (display " ")) stk)
    (newline)))

(define push! 
  (lambda (a) 
    (set! stk (cons a stk))))

(define pop! 
  (lambda () 
    (if (empty?)
        (error "pop!: The stack is empty.")
        (set! stk (cdr stk)))))

; - End Program -

; - Program 11.2, pg. 346 -

(define lookup   
  (lambda (obj table success-proc failure-proc)
    (letrec ((lookup (lambda (table)
                       (if (null? table)
                           (failure-proc)
                           (let ((pr (car table)))
                             (if (equal? (car pr) obj)
                                 (success-proc pr)
                                 (lookup (cdr table))))))))
      (lookup table))))        

; - End Program -

; - Program 11.3, pg. 347 -

(define assoc
  (lambda (obj table)
    (lookup obj table (lambda (pr) pr) (lambda () #f))))

; - End Program -

; - Program 11.4, pg. 347 -

(define memoize
  (lambda (proc)
    (let ((table '()))
      (lambda (arg)
        (lookup arg table 
          (lambda (pr) (cdr pr))
          (lambda ()
            (let ((val (proc arg)))
              (set! table (cons (cons arg val) table))
              val)))))))

; - End Program -

; - Program 11.5, pg. 348 -

(define memo-fib
  (memoize (lambda (n)
             (if (< n 2)
                 n
                 (+ (memo-fib (- n 1))
                    (memo-fib (- n 2)))))))  

; - End Program -

; - Program 11.6, pg. 350 -

(define vector-memoize
  (lambda (max-arg)
    (lambda (proc)
      (let ((table (make-vector (add1 max-arg) '())))
        (lambda (arg)
          (if (> arg max-arg)
              (proc arg)
              (let ((item-stored (vector-ref table arg)))
                (if (pair? item-stored)
                    (car item-stored)
                    (let ((val (proc arg)))
                      (vector-set! table arg (list val))
                      val)))))))))

; - End Program -

; - Program 11.7, pg. 352 -

(define member?
  (lambda (item ls)
    (let ((goto (lambda (label)
                  (label))))
      (letrec
        ((start
           (lambda ()
             (cond
               ((null? ls) #f)
               ((equal? (car ls) item) #t)
               (else (goto reduce)))))
         (reduce
           (lambda ()
             (set! ls (cdr ls))
             (goto start))))
        (goto start)))))

; - End Program -

; - Program 11.8, pg. 353 -

(define while-proc
  (lambda (pred-th body-th)
    (letrec ((loop (lambda ()
                     (if (pred-th) 
                         (begin 
                           (body-th) 
                           (loop))))))
      (loop))))

; - End Program -

; - Program 11.9, pg. 355 -

(define swapper
  (lambda (a b ls)
    (let ((ls* ls) (ans '()))
      (while-proc
        (lambda () (not (null? ls*)))
        (lambda ()
          (cond
            ((equal? (car ls*) a) (push! b))
            ((equal? (car ls*) b) (push! a))
            (else (push! (car ls*))))
;         (print-stack)
          (set! ls* (cdr ls*))))
      (while-proc
        (lambda () (not (empty?)))
        (lambda ()
          (set! ans (cons (top) ans))
;         (writeln "Answer = " ans)
          (pop!)
;         (print-stack)
                       ))
      ans)))

; - End Program -

; - Program 11.11, pg. 359 -

(define mystery
  (lambda (a b ls)
    (let ((ls* ls) (ans '()) (goto (lambda (label) (label))))
      (letrec
        ((push
           (lambda ()
             (cond
               ((null? ls*) (goto pop))
               ((eq? (car ls*) a) (push! b) (goto reduce))
               ((eq? (car ls*) b) (push! a) (goto reduce))
               (else (push! (car ls*)) (goto reduce)))))
         (reduce
           (lambda ()
             (set! ls* (cdr ls*))
             (goto push)))
         (pop
           (lambda ()
             (cond
               ((empty?) ans)
               (else
                 (set! ans (cons (top) ans))
                 (pop!)
                 (goto pop))))))
        (goto push)))))

; - End Program -

; - Program 11.21, pg. 368 -

(define last-pair
  (lambda (x)
    (if (pair? (cdr x))
        (last-pair (cdr x))
        x)))  

; - End Program -

; - Program 11.22, pg. 368 -

(define append!
  (lambda (ls1 ls2)
    (if (pair? ls1)
        (begin
          (set-cdr! (last-pair ls1) ls2)
          ls1)
        ls2)))

; - End Program -

; - Exercise 11.14, pg. 371 -

(define mystery
  (lambda(x)
    (let ((box (last-pair x)))
      (set-cdr! box x)
      x)))

; - End Exercise -

; - Exercise 11.16, pg. 371 -

(define efface
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) x) (cdr ls))
      (else (let ((z (efface x (cdr ls))))
              (set-cdr! ls z)
              ls)))))

(define test-efface
  (lambda ()
    (let ((x (cons 1 '())))
      (let ((y (cons 2 x)))
        (let ((z (cons 3 y)))
          (let ((a (cons 4 z)) (a* (cons 40 z)))
            (let ((b (cons 5 a)) (b* (cons 50 a)))
              (let ((c (cons 6 b)) (c* (cons 60 b)))
                (writeln x y z a a* b b* c c*)
                (efface 3 c)
                (writeln x y z a a* b b* c c*)))))))))

; - End Exercise -

; - Exercise 11.17, pg. 372 -

(define test-efface2
  (lambda ()
    (let ((ls (list 5 4 3 2 1)))
      (writeln (efface 3 ls))
      ls)))

(define test-efface3
  (lambda ()
    (let ((ls (list 5 4 3 2 1)))
      (writeln (efface 5 ls))
      ls)))

; - End Exercise -

; - Exercise 11.18, pg. 372 -

(define smudge
  (lambda (x ls)
    (letrec
      ((smudge/x
         (lambda (ls*)
           (cond
             ((null? (cdr ls*)) ls*)
             ((equal? (car ls*) x) (shift-down ls* (cdr ls*)))
             (else (smudge/x (cdr ls*)))))))
      (if (null? ls) 
          ls
          (begin
            (smudge/x ls)
            ls)))))

(define shift-down
  (lambda (box1 box2)
    (set-car! box1 (car box2))
    (set-cdr! box1 (cdr box2))))

(define test-smudge
  (lambda ()
    (let ((x (cons 1 '())))
      (let ((y (cons 2 x)))
        (let ((z (cons 3 y)))
          (let ((a (cons 4 z)) (a* (cons 40 z)))
            (let ((b (cons 5 a)) (b* (cons 50 a)))
              (let ((c (cons 6 b)) (c* (cons 60 b)))
                (writeln x y z a a* b b* c c*)
                (smudge 3 c)
                (writeln x y z a a* b b* c c*)))))))))

; - End Exercise -

; - Exercise 11.19, pg. 373 -

(define *seen-pairs* '())

(define count-pairs
  (lambda (pr)
    (if (dont-count? pr) 
        0
        (begin
          (set! *seen-pairs* (cons pr *seen-pairs*))
          (add1 (+ (count-pairs (car pr)) 
                   (count-pairs (cdr pr))))))))

(define dont-count?
  (lambda (s)
    (or (not (pair? s)) (member? s *seen-pairs*))))

(define test-count-pairs
  (lambda ()
    (let ((x (cons 'a (cons 'b (cons 'c '())))))
      (let ((y (cons x (cons x (cons x x)))))
        (set-cdr! (last-pair x) x)
        (writeln (count-pairs y))
        (count-pairs y)))))

(define count-pairs
  (lambda (pr)
    (count-pairs/seen pr '())))

; - End Exercise -

; - Exercise 11.20, pg. 375 -

(define reconfigure
  (lambda (tape character direction)
    (if (eq? direction 'left)
        (left (overwrite character tape))
        (right (overwrite character tape)))))

(define at
  (lambda (tape)
    (let ((right-part (2nd tape)))
      (car right-part))))

(define overwrite
  (lambda (char tape)
    (let ((left-part (1st tape)) (right-part (2nd tape)))
      (let ((new-right-part (cons char (cdr right-part))))
        (list left-part new-right-part)))))

(define right
  (lambda (tape)
    (let ((left-part (1st tape)) (right-part (2nd tape)))
      (let ((new-left-part (cons (car right-part) left-part))
            (new-right-part (cdr right-part)))
        (list new-left-part (check-null new-right-part))))))

(define check-null
  (lambda (part)
    (if (null? part)
        (list 0)
        part)))

(define test-reconfigure
  (lambda ()
    (let ((tape1 (list (list 'a 'b 'c 0) (list 'x 'y 0))))
      (let ((tape2 (reconfigure tape1 'u 'right))
            (tape3 (reconfigure tape1 'd 'left)))
        (let ((tape4 (reconfigure tape2 'v 'right))
              (tape5 (reconfigure tape3 'e 'left)))
          (let ((tape6 (reconfigure tape4 'w 'right))
                (tape7 (reconfigure tape5 'f 'left)))
            (let ((tape8 (reconfigure tape6 'x 'right))
                  (tape9 (reconfigure tape7 'g 'left)))
              (list tape8 tape9))))))))

; - End Exercise -

; - Exercise 11.22, pg. 378 -

(define shifter
  (letrec
    ((shift-to-0
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 0) tape)
             (else (shift-to-0 (reconfigure tape c 'right))))))))
    shift-to-0))

(define overwrite
  (lambda (char tape)
    (let ((right-part (2nd tape)))
      (set-car! right-part char)
      tape)))

; - End Exercise -

; - Exercise 11.23, pg. 379 -

(define busy-beaver
  (letrec
    ((loopright
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (loopright (reconfigure tape 'a 'right)))
             (else (maybe-done (reconfigure tape 'a 'right)))))))
     (maybe-done
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) (reconfigure tape 'a 'right))
             (else (continue (reconfigure tape 'a 'left)))))))
     (continue
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (maybe-done (reconfigure tape 'a 'left)))
             (else (loopright (reconfigure tape 'a 'right))))))))
    loopright))

(define endless-growth
  (letrec
    ((loop
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 0) 
              (loop (reconfigure tape 'a 'right))))))))
    loop))

(define perpetual-motion
  (letrec
    ((this-way
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (that-way (reconfigure tape 0 'right)))
             (else (that-way (reconfigure tape 'a 'right)))))))
     (that-way
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (this-way (reconfigure tape 0 'left)))
             (else (this-way (reconfigure tape 'a 'left))))))))
    this-way))

(define pendulum
  (letrec
    ((loopright
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (loopright (reconfigure tape 'a 'right)))
             (else (loopleft (reconfigure tape 'a 'left)))))))
     (loopleft
       (lambda (tape)
         (let ((c (at tape)))
           (cond
             ((equal? c 'a) 
              (loopleft (reconfigure tape 'a 'left)))
             (else (loopright (reconfigure tape 'a 'right))))))))
    loopright))

; - End Exercise -

; - Exercise 11.24, pg. 380 -

(define busy-beaver-lines
  '((loopright a a right loopright)
    (loopright 0 a right maybe-done)
    (maybe-done a a right halt)
    (maybe-done 0 a left continue)
    (continue a a left maybe-done)
    (continue 0 a right loopright)))

(define run-lines
  (lambda (lines tape)
    (letrec
      ((driver
         (lambda (state tape)
           (if (eq? state 'halt)
               tape
               (let ((matching-line 
                       (find-line state (at tape) lines)))
                 (driver
                   (next-state matching-line)
                   (reconfigure
                     tape 
                     (next-char matching-line)
                     (next-direction matching-line))))))))
      (driver (current-state (car lines)) tape))))

; - End Exercise -

; - Exercise 11.26, pg. 382 -

(define overwrite
  (lambda (char tape)
    (let ((left (2nd tape)) (right (3rd tape)))
      (list char left right))))

(define right
  (lambda (tape)
    (let ((char (1st tape)) (left (2nd tape)) (right (3rd tape)))
      (list (car right) (cons char left) (check-null (cdr right))))))

; - End Exercise -

