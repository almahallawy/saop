; - Program 6.1, pg. 165 -

(define string-insert
  (lambda (insrt strng n)
    (string-append
      (substring strng 0 n)
      insrt
      (substring strng n (string-length strng)))))

; - End Program -

; - Exercise 6.4, pg. 167 -

(define mystery
  (lambda (pos-int)
    (letrec ((helper
               (lambda (n count)
                 (cond
                   ((= n 1) 
                    (newline)
                    (writeln "It took " count " steps to get to 1."))
                   ((even? n)
                    (writeln count 
                      ". We divide " n " by 2.")
                    (helper (/ n 2) (add1 count)))
                   (else 
                    (writeln count
                      ". We multiply " n " by 3 and add 1.")
                    (helper (+ (* n 3) 1) (add1 count)))))))
      (helper pos-int 0))))

; - End Exercise -

; - Program 6.2, pg. 169 -

(define square-root
  (lambda (a)
    (letrec 
      ((next-estimate 
         (lambda (u)
           (let ((v (/ (+ u (/ a u)) 2)))
             (if (close-enough? u v)
                 v
                 (next-estimate v))))))
      (next-estimate 1))))

; - End Program -  

; - Program 6.3, pg. 171 -

(define square-root-display
  (lambda (a)
    (letrec ((next-estimate (lambda (u)
                              (let ((v (/ (+ u (/ a u)) 2)))
                                (if (close-enough? u v)
                                    v
                                    (begin
                                      (display v)
                                      (newline)
                                      (next-estimate v)))))))
      (next-estimate 1)))) 

; - End Program -

; - Program 6.5, pg. 172 -

(define round-n-places
  (lambda (n dec-num)
    (let ((scale-factor (expt 10 n)))
      (/ (round (* dec-num scale-factor)) scale-factor)))) 

; - End Program -

; - Program 6.6, pg. 174 -

(define read-demo
  (lambda ()
    (display "Enter data (enter done when finished): ")
    (let ((response (read)))
      (cond
        ((eq? response 'done) (display "Thank you. Good-bye."))
        (else (display "You entered: ")
              (write response)
              (newline)
              (read-demo))))))

; - End Program -

; - Program 6.7, pg. 175 -

(define interactive-square-root
  (lambda ()
    (writeln "Enter the number whose square root you want,"
             " or enter done to quit:")
    (let ((n (read)))
      (if (eqv? n 'done)
          (writeln "That's all, folks.")
          (begin
            (writeln "The square root of " n " is " (square-root n))
            (newline)
            (interactive-square-root))))))

; - End Program -

; - Program 6.9, pg. 181 -

(define tower-of-hanoi
  (lambda (n)
    (letrec 
      ((move 
         (lambda (n source destination helper)
           (if (= n 1) 
               (list (list source destination))
               (append
                 (move (sub1 n) source helper destination)
                 (cons
                   (list source destination)
                   (move (sub1 n) helper destination source)))))))
      (move n 'L 'R 'C))))

; - End Program -

; - Program 6.10, pg. 182 -

(define display-tower-of-hanoi
  (let ((show-move (lambda (s d) 
                     (display s)
                     (display " -> ")
                     (display d))))
    (lambda (n)
      (letrec 
        ((move 
           (lambda (n source destination helper)
             (if (= n 1) 
                 (begin
                   (show-move source destination) 
                   (newline))
                 (begin
                   (move (sub1 n) source helper destination)
                   (show-move source destination)
                   (display ", ")
                   (move (sub1 n) helper destination source))))))
        (move n 'L 'R 'C)))))

; - End Program -

; - Program 6.12, pg. 184 -

(define legal?
  (lambda (try legal-pl)
    (letrec
      ((good?
         (lambda (new-pl up down)
           (cond
             ((null? new-pl) #t)
             (else (let ((next-pos (car new-pl)))
                     (and
                       (not (= next-pos try))
                       (not (= next-pos up))
                       (not (= next-pos down))
                       (good? (cdr new-pl) 
                              (add1 up) 
                              (sub1 down)))))))))
      (good? legal-pl (add1 try) (sub1 try)))))

(define solution?
  (lambda (legal-pl)
    (= (length legal-pl) 8)))

(define fresh-try 8)

; - End Program -

; - Program 6.13, pg. 185 -

(define build-solution
  (lambda (legal-pl)
    (cond
      ((solution? legal-pl) legal-pl)
      (else (forward fresh-try legal-pl)))))

; - End Program -

; - Program 6.14, pg. 186 -

(define forward
  (lambda (try legal-pl)
    (cond
      ((zero? try) (backtrack legal-pl))
      ((legal? try legal-pl) (build-solution (cons try legal-pl)))
      (else (forward (sub1 try) legal-pl)))))

; - End Program -

; - Program 6.15, pg. 186 -

(define backtrack
  (lambda (legal-pl)
    (cond
      ((null? legal-pl) '())
      (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))

; - End Program -

; - Program 6.16, pg. 188 -

(define searcher
  (lambda (legal? solution? fresh-try)
    (letrec 
      ((build-solution
         (lambda (legal-pl)
           (cond
             ((solution? legal-pl) legal-pl)
             (else (forward fresh-try legal-pl)))))
       (forward
         (lambda (try legal-pl)
           (cond
             ((zero? try) (backtrack legal-pl))
             ((legal? try legal-pl) 
              (build-solution (cons try legal-pl)))
             (else (forward (sub1 try) legal-pl)))))
       (backtrack
         (lambda (legal-pl)
           (cond
             ((null? legal-pl) '())
             (else 
               (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
       (build-all-solutions
         (lambda ()
           (letrec
             ((loop (lambda (sol)
                      (cond
                        ((null? sol) '())
                        (else (cons sol (loop (backtrack sol))))))))
             (loop (build-solution '()))))))
      (build-all-solutions))))

; - End Program -

; - Exercise 6.16, pg. 191 -

(define blanks
  (lambda (n)
    (cond
      ((zero? n) "")
      (else (string-append " " (blanks (sub1 n)))))))

; - End Exercise -
