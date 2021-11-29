; - Exercise 16.6, pg. 526 -

(define reset
  (lambda ()
    ((escaper
       (lambda ()
         (writeln "reset invoked"))))))

; - End Exercise -

; - Program 16.1, pg. 527 -

(define call/cc call-with-current-continuation)

; - End Program -

; - Program 16.2, pg. 530 -

(define receiver-1 
  (lambda (proc) 
    (proc (list 1))))

(define receiver-2 
  (lambda (proc) 
    (proc (list (proc (list 2))))))

(define receiver-3 
  (lambda (proc) 
    (proc (list (proc (list 3 proc))))))

; - End Program -

; - Program 16.3, pg. 531 -

(define result "any value")

(define resultcc "any value")

; - End Program -

; - Program 16.4, pg. 531 -

(define writeln/return
  (lambda (x)
    (writeln x)
    x))

(define answer-maker
  (lambda (x)
    (cons 'answer-is (writeln/return x))))

(define call
  (lambda (receiver)
    (receiver writeln/return)))

; - End Program -

; - Exercise 16.16, pg. 537 -

(define deep "any continuation")

(define map-sub1
  (lambda (ls)
    (if (null? ls)
        (let ((receiver (lambda (k) 
                          (set! deep k) 
                          '())))
          (call/cc receiver))
        (cons (sub1 (car ls)) (map-sub1 (cdr ls))))))

; - End Exercise -

; - Program 16.5, pg. 538 -

(define *escape/thunk* "any continuation")

(define escaper
  (lambda (proc)
    (lambda (x)
      (*escape/thunk* (lambda () (proc x))))))

; - End Program -

; - Program 16.6, pg. 539 -

(define receiver-4
  (lambda (continuation)
    (set! *escape/thunk* continuation)
    (*escape/thunk* (lambda () (writeln "escaper is defined")))))

; - End Program -

; - Program 16.7, pg. 540 -

(define escaper
  (lambda (proc)
    (lambda args
      (*escape/thunk* 
        (lambda ()
          (apply proc args))))))

; - End Program -

; - Exercise 16.22, pg. 541 -

(define new-escaper "any procedure")

(let ((receiver (lambda (continuation)
                  (set! new-escaper
                    (lambda (proc)
                      (lambda args
                        (continuation
                          (lambda ()
                            (apply proc args))))))
                  (lambda () (writeln "new-escaper is defined")))))
  ((call/cc receiver)))

; - End Exercise -

; - Program 16.8, pg. 542 -

(define how-many-til
  (lambda (n target)
    (let ((count 0))
      (cycle-proc
        (lambda ()
          (let ((r (random n)))
            (if (= r target)
                (begin (writeln count) (set! count 0))
                (set! count (+ count 1)))))))))

; - End Program -

; - Program 16.9, pg. 543 -

(define how-many-til
  (lambda (n target thresh)
    (let ((receiver
            (lambda (exit-above-threshold)
              (let ((count 0) (sum 0))
                (cycle-proc
                  (lambda ()
                    (if (= (random n) target)
                        (begin
                          (writeln "target " target 
                                   " required " count " trials")
                          (set! sum (+ sum count))
                          (set! count 0)
                          (if (> sum thresh) 
                              (exit-above-threshold sum)))
                        (set! count (+ count 1)))))))))
      (call/cc receiver))))

; - End Program -

; - Program 16.10, pg. 543 -

(define random-data
  (lambda (n thresh)
    (letrec ((loop (lambda (target)
                     (cond
                       ((negative? target) '())
                       (else (cons (how-many-til n target thresh)
                                   (loop (sub1 target))))))))
      (loop (sub1 n)))))

; - End Program -

; - Program 16.11, pg. 544 -

(define product+
  (lambda (n nums)
    (letrec
      ((product (lambda (nums)
                  (cond
                    ((null? nums) 1)
                    (else (* (car nums) (product (cdr nums))))))))
      (let ((prod (product nums)))
        (if (zero? prod) 0 (+ n prod))))))

; - End Program -

; - Program 16.12, pg. 545 -

(define product+
  (lambda (n nums)
    (letrec
      ((product (lambda (nums)
                  (cond
                    ((null? nums) 1)
                    ((zero? (car nums)) 0)
                    (else (* (car nums) (product (cdr nums))))))))
      (let ((prod (product nums)))
        (if (zero? prod) 0 (+ n prod))))))

; - End Program -

; - Program 16.13, pg. 546 -

(define product+
  (lambda (n nums)
    (let ((receiver
            (lambda (exit-on-zero)
              (letrec
                ((product (lambda (nums)
                            (cond
                              ((null? nums) 1)
                              ((zero? (car nums)) (exit-on-zero 0))
                              (else (* (car nums) 
                                       (product (cdr nums))))))))
                (let ((prod (product nums)))
                  (if (zero? prod) 0 (+ n prod)))))))
      (call/cc receiver))))

; - End Program -

; - Program 16.14, pg. 546 -

(define product+
  (lambda (n nums)
    (let ((receiver
            (lambda (exit-on-zero)
              (letrec
                ((product (lambda (nums)
                            (cond
                              ((null? nums) 1)
                              ((zero? (car nums)) (exit-on-zero 0))
                              (else (* (car nums) 
                                       (product (cdr nums))))))))
                (+ n (product nums))))))
      (call/cc receiver))))

; - End Program -

; - Program 16.15, pg. 547 -

(define product+
  (lambda (n nums)
    (let ((receiver
            (lambda (exit-on-zero)
              (letrec
                ((product
                   (lambda (nums)
                     (cond
                       ((null? nums) 1)
                       ((number? (car nums))
                        (cond
                          ((zero? (car nums)) (exit-on-zero 0))
                          (else (* (car nums) 
                                   (product (cdr nums))))))
                       (else (* (product (car nums)) 
                                (product (cdr nums))))))))
                (+ n (product nums))))))
      (call/cc receiver))))

; - End Program -

; - Program 16.16, pg. 547 -

(define *-and-count-maker
  (lambda ()
    (let ((local-counter 0))
      (lambda (n1 n2)
        (set! local-counter (+ local-counter 1))
        (writeln "Number of multiplications = " local-counter)
        (* n1 n2)))))

; - End Program -

; - Program 16.17, pg. 548 -

(define product+
  (lambda (n nums *-proc)
    (letrec 
      ((product 
         (lambda (nums)
           (cond
             ((null? nums) 1)
             ((number? (car nums))
              (cond
                ((zero? (car nums)) 0)
                (else (*-proc (car nums) 
                              (product (cdr nums))))))
             (else (*-proc (product (car nums)) 
                           (product (cdr nums))))))))
      (let ((prod (product nums)))
        (if (zero? prod) 0 (+ n prod))))))

; - End Program -

