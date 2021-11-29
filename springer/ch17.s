; - Program 17.1, pg. 553 -

(define countdown
  (lambda (n)
    (writeln "This only appears once")
    (let ((pair (message "Exit" (attempt (message "Enter" n)))))
      (let ((v (1st pair))
            (returner (2nd pair)))
        (writeln "    The non-negative-number: " v)
        (if (positive? v) 
            (returner (list (sub1 v) returner))
            (writeln "Blastoff"))))))

; - End Program -

; - Program 17.2, pg. 554 -

(define message
  (lambda (direction value)
    (writeln "   " direction "ing attempt with value: " value)
    value))

; - End Program -

; - Program 17.3, pg. 554 -

(define attempt
  (lambda (n)
    (let ((receiver (lambda (proc) (list n proc))))
      (receiver (lambda (x) x)))))

; - End Program -

; - Program 17.4, pg. 555 -

(define attempt
  (lambda (n)
    (let ((receiver (lambda (proc) (list n proc))))
      (call/cc receiver))))

; - End Program -

; - Program 17.5, pg. 557 -

(define receiver
  (lambda (continuation)
    (continuation continuation)))

; - End Program -

; - Program 17.6, pg. 557 -

(define tester
  (lambda (continuation)
    (writeln "beginning")
    (call/cc continuation)
    (writeln "middle")
    (call/cc continuation)
    (writeln "end")))

; - End Program -

; - Program 17.7, pg. 560 -

(define flatten-number-list
  (lambda (s)
    (letrec
      ((flatten
         (lambda (s)
           (cond
             ((null? s) '())
             ((number? s) (list (break s)))
             (else (let ((flatcar (flatten (car s))))
                     (append flatcar (flatten (cdr s)))))))))
      (flatten s))))

; - End Program -

; - Program 17.8, pg. 560 -

(define break
  (lambda (x) x))

; - End Program -

; - Program 17.9, pg. 560 -

(define break
  (lambda (x)
    (let ((break-receiver
            (lambda (continuation)
              (continuation x))))
      (call/cc break-receiver))))

; - End Program -

; - Program 17.10, pg. 561 -

(define get-back "any procedure")            

(define break
  (lambda (x)
    (let ((break-receiver
            (lambda (continuation)
              (set! get-back (lambda () (continuation x)))
              (any-action x))))
      (call/cc break-receiver))))

; - End Program -

; - Program 17.11, pg. 561 -

(define any-action
  (lambda (x)
    (writeln x)
    (get-back)))

; - End Program -

; - Program 17.12, pg. 561 -

(define any-action
  (lambda (x)
    ((escaper (lambda () x)))
    (get-back)))

; - End Program -

; - Program 17.13, pg. 562 -

(define get-back "any escape procedure")      

(define break
  (lambda (x)
    (let ((break-receiver
            (lambda (continuation)
              (set! get-back continuation)
              (any-action x))))
      (call/cc break-receiver))))

; - End Program -

; - Program 17.14, pg. 563 -

(define break-argument "any value")

(define any-action
  (lambda (x)
    (set! break-argument x)
    ((escaper (lambda () x)))))

; - End Program -

; - Program 17.15, pg. 564 -

(define get-back "any escape procedure")

(define break
  (lambda (x)
    (let ((break-receiver
            (lambda (continuation)
              (set! get-back continuation)
              (set! break-argument x)
              ((escaper (lambda () x))))))
      (call/cc break-receiver))))

; - End Program -

; - Program 17.16, pg. 564 -

(define flatten-number-list
  (lambda (s)
    (letrec
      ((flatten
         (lambda (s)
           (cond
             ((null? s) '())
             ((number? s) 
              (list
                (break 
                  (list (lambda () s)
                        (lambda (v) (set! s v))))))
             (else (let ((flatcar (flatten (car s))))
               (append flatcar (flatten (cdr s)))))))))
      (flatten s))))

; - End Program -

; - Program 17.17, pg. 564 -

(define extract
  (lambda ()
    ((1st break-argument))))

; - End Program -

; - Program 17.18, pg. 565 -

(define store
  (lambda (value)
    ((2nd break-argument) value)))

; - Exercise 17.4, pg. 565 -

(define flatten-number-list
  (lambda (s)
    (letrec
      ((flatten
         (lambda (s)
           (cond
             ((null? s) '())
             ((number? s) (break (list s)))
             (else (let ((flatcar (flatten (car s))))
                     (append flatcar (flatten (cdr s)))))))))
      (flatten s))))

; - End Exercise -

; - Exercise 17.6, pg. 565 -

(define product+
  (lambda (n nums)
    (letrec
        ((product 
           (lambda (nums)
             (cond
               ((null? nums) 1)
               ((number? (car nums))
                (* (cond
                     ((zero? (car nums)) (break-on-zero))
                     (else (car nums)))
                   (product (cdr nums))))
               (else (* (product (car nums)) (product (cdr nums))))))))
      (+ n (product nums)))))

; - End Exercise -

; - Program 17.19, pg. 569 -

(define A
  (let ((A-proc (lambda (resume v)
                  (writeln "This is A")
                  (writeln "Came from " (resume B "A"))
                  (writeln "Back in A")
                  (writeln "Came from " (resume C "A")))))
    (coroutine-maker A-proc)))

(define B
  (let ((B-proc (lambda (resume v)
                  (writeln (blanks 14) "This is B")
                  (writeln (blanks 14) 
                           "Came from " (resume C "B"))
                  (writeln (blanks 14) "Back in B")
                  (writeln (blanks 14) 
                           "Came from " (resume A "B")))))
    (coroutine-maker B-proc)))

(define C
  (let ((C-proc (lambda (resume v)
                  (writeln (blanks 28) "This is C")
                  (writeln (blanks 28) 
                           "Came from " (resume A "C"))
                  (writeln (blanks 28) "Back in C")
                  (writeln (blanks 28) 
                           "Came from " (resume B "C")))))
    (coroutine-maker C-proc)))

; - End Program -

; - Program 17.20, pg. 570 -

(define coroutine-maker
  (lambda (proc)
    (let ((saved-continuation "any continuation"))
      (let ((update-continuation! 
              (lambda (v) 
                (set! saved-continuation v))))
        (let ((resumer (resume-maker update-continuation!))
              (first-time #t))
          (lambda (value)
            (if first-time
                (begin
                  (set! first-time #f)
                  (proc resumer value))
                (saved-continuation value))))))))

; - End Program -

; - Program 17.21, pg. 570 -

(define resume-maker
  (lambda (update-proc!)
    (lambda (next-coroutine value)
      (let ((receiver (lambda (continuation)
                        (update-proc! continuation)
                        (next-coroutine value))))
        (call/cc receiver)))))

; - End Program -

; - Exercise 17.12, pg. 571 -

(define ping
  (let ((ping-proc (lambda (resume v)
                     (display "ping-")
                     (resume pong 'ignored-ping))))
    (coroutine-maker ping-proc)))



(define pong
  (let ((pong-proc (lambda (resume v)
                     (display "pong")
                     (newline)
                     (resume ping 'ignored-pong))))
    (coroutine-maker pong-proc)))

; - End Exercise -

; - Program 17.22, pg. 573 -

(define reader
  (lambda (right)
    (let ((co-proc (lambda (resume v)
                     (cycle-proc 
                       (lambda ()
                         (resume right (prompt-read "in> ")))))))
      (coroutine-maker co-proc))))

; - End Program -

; - Program 17.23, pg. 574 -

(define writer
  (lambda (left escape-on-end)
    (let ((co-proc (lambda (resume v)
                     (cycle-proc
                       (lambda ()
                         (let ((symbol (resume left 'ok)))
                           (if (eq? symbol 'end)
                               (escape-on-end symbol)
                               (writeln "out> " symbol))))))))
      (coroutine-maker co-proc))))

; - End Program -

; - Program 17.24, pg. 574 -

(define x->y
  (lambda (x y left right)
    (let ((co-proc (lambda (resume v)
                     (cycle-proc 
                       (lambda ()
                         (let ((symbol-1 (resume left 'ok)))
                           (if (eq? x symbol-1)
                               (let ((symbol-2 (resume left 'more)))
                                 (if (eq? x symbol-2)
                                     (resume right y)
                                     (begin
                                       (resume right symbol-1)
                                       (resume right symbol-2))))
                               (resume right symbol-1))))))))
      (coroutine-maker co-proc))))

; - End Program -

; - Program 17.25, pg. 575 -

(define grune
  (lambda ()
    (let ((grune-receiver 
            (lambda (escape-grune) 
              (letrec
                ((Input (reader (lambda (v) (A v))))
                 (A (x->y 'a 'b (lambda (v) (Input v)) (lambda (v) (B v))))
                 (B (x->y 'b 'c (lambda (v) (A v)) (lambda (v) (Output v))))
                 (Output (writer (lambda (v) (B v)) escape-grune)))
                (Output 'ok)))))
      (call/cc grune-receiver))))

; - End Program -

; - Exercise 17.16, pg. 576 -

(define grune
  (lambda ()
    (let ((grune-receiver (lambda (escape-grune) 
                            (safe-letrec
                              ((Input (reader A))
                               (A (x->y 'a 'b Input B))
                               (B (x->y 'b 'c A Output))
                               (Output (writer B escape-grune)))
                              (Output 'ok)))))
      (call/cc grune-receiver))))

; - End Exercise -

; - Exercise 17.17, pg. 577 -

(define process-maker
  (lambda (f)
    (let ((saved-continuation "any continuation"))
      (let ((update-continuation! 
              (lambda (v) 
                (set! saved-continuation v))))
        (let ((resumer (resume-maker update-continuation!))
              (first-time #t))
          (lambda (value)
            (if first-time
                (begin
                  (set! first-time #f)
                  (cycle-proc
                    (lambda ()
                      (f resumer value))))
                (saved-continuation value))))))))

; - End Exercise -

