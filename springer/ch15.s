; - Program 15.1, pg. 477 -

(define delayed-list-car (compose car force))

; - End Program -

; - Program 15.2, pg. 478 -

(define delayed-list-cdr (compose cdr force))

; - End Program -

; - Program 15.3, pg. 478 -

(define the-null-delayed-list (delay '()))

; - End Program -

; - Program 15.4, pg. 479 -

(define random-delayed-list
  (lambda (n)
    (if (zero? n)
        the-null-delayed-list
        (delayed-list-cons 
          (+ 2 (random 11)) 
          (random-delayed-list (sub1 n))))))

; - End Program -

; - Program 15.5, pg. 483 -

(define stream-car (compose car force))

(define stream-cdr (compose cdr force))

; - End Program -

; - Program 15.6, pg. 483 -

(define random-stream-generator
  (lambda ()
    (stream-cons (+ 2 (random 11)) (random-stream-generator))))

; - End Program -

; - Program 15.7, pg. 483 -

(define random-stream (random-stream-generator))

; - End Program -

; - Program 15.8, pg. 484 -

(define the-null-stream
  (stream-cons the-end-of-stream-tag the-null-stream))

; - End Program -

; - Program 15.9, pg. 484 -

(define list->stream
  (lambda (ls)
    (if (null? ls)
        the-null-stream
        (stream-cons (car ls) (list->stream (cdr ls))))))

; - End Program -

; - Program 15.10, pg. 484 -

(define end-of-stream?
  (lambda (x)
    (eq? x the-end-of-stream-tag)))

; - End Program -

; - Program 15.11, pg. 485 -

(define stream-null? (compose end-of-stream? stream-car))

; - End Program -

; - Program 15.12, pg. 485 -

(define stream->list
  (lambda (strm n)
    (if (or (stream-null? strm) (zero? n))
        '()
        (cons (stream-car strm) 
              (stream->list (stream-cdr strm) (sub1 n))))))

(define finite-stream->list
  (lambda (finite-strm)
    (stream->list finite-strm -1)))

; - End Program -

; - Program 15.13, pg. 486 -

(define positive-integers
  (letrec 
    ((stream-builder
       (lambda (x)
         (stream-cons x (stream-builder (add1 x))))))
    (stream-builder 1)))

; - End Program -

; - Program 15.14, pg. 486 -

(define even-positive-integers
  (letrec
    ((stream-builder
       (lambda (x)
         (stream-cons x (stream-builder (+ x 2))))))
    (stream-builder 2)))

; - End Program -

; - Program 15.15, pg. 486 -

(define powers-of-2
  (letrec
    ((stream-builder
       (lambda (x)
         (stream-cons x (stream-builder (* x 2))))))
    (stream-builder 1)))

; - End Program -

; - Program 15.16, pg. 487 -

(define build-stream
  (lambda (seed proc)
    (letrec 
      ((stream-builder
         (lambda (x)
           (stream-cons x (stream-builder (proc x))))))
      (stream-builder seed))))

; - End Program -

; - Program 15.17, pg. 488 -

(define factorials
  (letrec
    ((stream-builder
       (lambda (x n)
         (stream-cons x (stream-builder (* x n) (add1 n))))))
    (stream-builder 1 1)))

; - End Program -

; - Program 15.18, pg. 488 -

(define stream-map
  (lambda (proc strm)
    (stream-cons 
      (proc (stream-car strm)) 
      (stream-map proc (stream-cdr strm)))))

; - End Program -

; - Program 15.19, pg. 489 -

(define odd-positive-integers
  (stream-map sub1 even-positive-integers))  

; - End Program -

; - Program 15.20, pg. 489 -

(define stream-apply-to-both
  (lambda (proc)
    (letrec 
      ((str-app
         (lambda (s1 s2)
           (stream-cons 
             (proc (stream-car s1) (stream-car s2))
             (str-app (stream-cdr s1) (stream-cdr s2))))))
      str-app)))

; - End Program -

; - Program 15.21, pg. 489 -

(define stream-plus (stream-apply-to-both +))

(define stream-times (stream-apply-to-both *))

; - End Program -

; - Program 15.22, pg. 490 -

(define stream-filter-out
  (lambda (test?)
    (letrec 
      ((helper
         (lambda (strm)
           (let ((a (stream-car strm)))
             (if (test? a)
                 (helper (stream-cdr strm))
                 (stream-cons a (helper (stream-cdr strm))))))))
       helper)))

; - End Program -

; - Program 15.23, pg. 490 -

(define positive-integers
  (stream-cons 1 (stream-map add1 positive-integers)))  

; - End Program -

; - Program 15.24, pg. 491 -

(define factorials
  (stream-cons 1 (stream-times factorials positive-integers)))  

; - End Program -

; - Program 15.25, pg. 491 -

(define fibonacci-numbers
  (stream-cons 0
    (stream-cons 1 
      (stream-plus 
        fibonacci-numbers
        (stream-cdr fibonacci-numbers))))) 

; - End Program -

; - Program 15.26, pg. 492 -

(define sieve
  (lambda (n strm)
    ((stream-filter-out (divides-by n)) strm))) 

; - End Program -

; - Program 15.27, pg. 492 -

(define prime-numbers
  (letrec 
    ((primes
       (lambda (s)
         (stream-cons 
           (stream-car s) 
           (primes (sieve (stream-car s) (stream-cdr s)))))))      
    (primes (stream-cdr positive-integers))))

; - End Program -

; - Exercise 15.14, pg. 495 -

(define stream-append
  (lambda (finite-stream stream)
    (cond
      ((stream-null? finite-stream) stream)
      (else (stream-cons 
              (stream-car finite-stream) 
              (stream-append 
                (stream-cdr finite-stream) 
                stream))))))

(define int-pairs-generator
  (lambda (i)
    (stream-append (diagonal i) (int-pairs-generator (add1 i)))))

(define stream-append/delay
  (lambda (finite-stream stream)
    (cond
      ((stream-null? finite-stream) (force stream))
      (else (stream-cons 
              (stream-car finite-stream) 
              (stream-append/delay 
                (stream-cdr finite-stream) 
                stream))))))

; - End Exercise -

; - Exercise 15.17, pg. 499 -

(define string-tester
  (lambda (str)
    (let ((chars (string->list str)))
      (let ((s (list->string chars)))
        (write (list s chars))
        (newline)))))

; - End Exercise -

; - Program 15.29, pg. 504 -

(define file-copier
  (lambda (infile outfile)
    (let ((p-in (open-input-file infile))
          (p-out (open-output-file outfile)))
      (letrec
        ((copier (lambda (ch)
                   (if (not (eof-object? ch))
                       (begin
                         (write-char ch p-out)
                         (copier (read-char p-in)))))))
        (copier (read-char p-in))
        (close-input-port p-in)
        (close-output-port p-out)))))

; - End Program -

; - Program 15.30, pg. 505 -

(define file->stream
  (lambda (filename)
    (let ((port-in (open-input-file filename)))
      (letrec
        ((build-input-stream
           (lambda ()
             (let ((ch (read-char port-in)))
               (if (eof-object? ch)
                   (begin
                     (close-input-port port-in)
                     the-null-stream)
                   (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

; - End Program -

; - Program 15.31, pg. 506 -

(define formatter
  (lambda (input-file output-file line-length)
    (stream->file output-file
      (insert-newlines line-length
        (insert-double-spaces
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-file))))))))

; - End Program -

; - Program 15.32, pg. 506 -

(define remove-newlines
  (lambda (str)
    (stream-map
      (lambda (ch)
        (cond 
          ((end-of-stream? ch) ch)
          ((or (char=? ch #\return) (char=? ch #\newline)) #\space)
          (else ch)))
      str)))

; - End Program -

; - Program 15.33, pg. 506 -

(define remove-extra-spaces
  (lambda (str)
    (let ((ch (stream-car str)))
      (cond 
        ((end-of-stream? ch) str)
        ((char=? ch #\space) 
         (stream-cons #\space 
           (remove-extra-spaces
             (trim-spaces (stream-cdr str)))))
        (else (stream-cons 
                ch 
                (remove-extra-spaces (stream-cdr str))))))))

; - End Program -

; - Program 15.34, pg. 507 -

(define trim-spaces
  (lambda (str)
    (cond 
      ((stream-null? str) str)
      ((char=? (stream-car str) #\space)
       (trim-spaces (stream-cdr str)))
      (else str))))

; - End Program -

; - Program 15.35, pg. 507 -

(define insert-double-spaces
  (lambda (str)
    (let ((ch (stream-car str)))    
      (cond 
        ((end-of-stream? ch) str)
        ((end-of-sentence? ch)
         (stream-cons ch
           (stream-cons #\space
             (stream-cons #\space
               (insert-double-spaces 
                 (trim-spaces (stream-cdr str)))))))
        (else (stream-cons ch
                (insert-double-spaces (stream-cdr str))))))))

; - End Program -

; - Program 15.36, pg. 507 -

(define end-of-sentence?
  (lambda (ch)
    (or (char=? ch #\.) (char=? ch #\!) (char=? ch #\?))))

; - End Program -

; - Program 15.37, pg. 508 -

(define insert-newlines
  (lambda (line-length str)
    (letrec 
      ((insert (lambda (str count)
                 (if (stream-null? str)
                     str
                     (let ((n (count-chars-to-next-space str)))
                       (if (< (+ n count) line-length)
                           (stream-cons (stream-car str)
                             (insert (stream-cdr str) (add1 count)))
                           (stream-cons #\newline
                             (insert (trim-spaces str) 0))))))))
      (insert (trim-spaces str) 0))))

; - End Program -

; - Program 15.38, pg. 508 -

(define count-chars-to-next-space
  (lambda (strm)
    (letrec
      ((count-ahead
         (lambda (str count)
           (let ((ch (stream-car str)))
             (if (or (end-of-stream? ch) (char=? ch #\space))
                 count
                 (count-ahead (stream-cdr str) (add1 count)))))))
      (count-ahead strm 0))))

; - End Program -

; - Program 15.39, pg. 509 -

(define stream->file
  (lambda (filename stream)
    (let ((port-out (open-output-file filename)))
      (letrec ((write-stream
                 (lambda (str)
                   (if (not (stream-null? str))
                       (begin
                         (write-char (stream-car str) port-out)
                         (write-stream (stream-cdr str)))))))
        (write-stream stream)
        (close-output-port port-out)))))

; - End Program -

; - Exercise 15.28, pg. 510 -

(define formatter
  (lambda (input-file output-file line-length)
    ((stream->file output-file)
     ((insert-newlines line-length)
      (insert-double-spaces
        (remove-extra-spaces
          (remove-newlines
            (file->stream input-file))))))))

; - End Exercise -

; - Exercise 15.29, pg. 511 -

(define formatter
  (lambda (output-file line-length)
    (lambda (input-file)
      ((stream->file output-file)
       ((insert-newlines line-length)
        (insert-double-spaces
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-file)))))))))

; - End Exercise -

; - Exercise 15.30, pg. 511 -

(define apply-procedures
  (lambda (procedures)
    (lambda (argument)
      (if (null? procedures)
          argument
          (letrec 
            ((comp (lambda (procs)
                     ((car procs)
                      (cond
                        ((null? (cdr procs)) argument)
                        (else (comp (cdr procs))))))))
            (comp procedures))))))

; - End Exercise -

; - Exercise 15.31, pg. 511 -

(define formatter
  (lambda (output-file line-length)
    (compose
      (stream->file output-file)
      (insert-newlines line-length)
      insert-double-spaces      
      remove-extra-spaces
      remove-newlines
      file->stream)))

; - End Exercise -

