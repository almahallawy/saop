; - Program 10.1, pg. 304 -

(define insertsort 
  (lambda (ls)
    (if (singleton-list? ls) 
        ls 
        (insert (car ls) (insertsort (cdr ls)))))) 

; - End Program -

; - Program 10.2, pg. 304 -

(define insert
  (lambda (a ls) 
    (cond
      ((null? ls) (cons a '()))
      ((< a (car ls)) (cons a ls))
      (else (cons (car ls) (insert a (cdr ls))))))) 

; - End Program -

; - Program 10.5, pg. 306 -

(define vector-insertsort!  
  (lambda (v) 
    (let ((size (vector-length v)))
      (letrec ((sortloop (lambda (k) 
                           (if (< k size) 
                               (begin 
                                 (vector-insert! k v) 
                                 (sortloop (add1 k))))))) 
        (sortloop 1))))) 

; - End Program -

; - Program 10.6, pg. 307 -

(define vector-insert!  
  (lambda (k vec) 
    (let ((val (vector-ref vec k)))
      (letrec ((insert-h 
                 (lambda (m) 
                   (if (zero? m) 
                       (vector-set! vec 0 val)
                       (let ((comp (vector-ref vec (sub1 m)))) 
                         (if (< val comp) 
                             (begin
                               (vector-set! vec m comp) 
                               (insert-h (sub1 m))) 
                             (vector-set! vec m val))))))) 
        (insert-h k))))) 

; - End Program -

; - Program 10.7, pg. 310 -

(define make-groups 
  (lambda (ls) 
    (cond 
      ((null? ls) '()) 
      ((null? (cdr ls)) (list ls)) 
      (else (let ((a (car ls)) 
                  (gp (make-groups (cdr ls)))) 
              (if (< (cadr ls) a) 
                  (cons (list a) gp) 
                  (cons (cons a (car gp)) (cdr gp)))))))) 

; - End Program -

; - Program 10.8, pg. 311 -

(define pair-merge 
  (lambda (sublists) 
    (cond 
      ((null? sublists) '()) 
      ((null? (cdr sublists)) sublists) 
      (else (cons (merge (car sublists) (cadr sublists))
                  (pair-merge (cddr sublists))))))) 

; - End Program -

; - Program 10.9, pg. 311 -

(define nat-mergesort
  (lambda (ls) 
    (if (null? ls) 
        '() 
        (letrec ((sort (lambda (gps) 
                         (if (null? (cdr gps))
                             (car gps) 
                             (sort (pair-merge gps)))))) 
          (sort (make-groups ls)))))) 

; - End Program -

; - Program 10.10, pg. 313 -

(define vector-merge!
  (lambda (newvec vec)
    (lambda (left top-left right top-right)
      (letrec
        ((mergeloop
           (lambda (left right i)
             (cond
               ((and (< left top-left) (< right top-right))
                (if (< (vector-ref vec left) (vector-ref vec right))
                    (begin
                      (vector-set! newvec i (vector-ref vec left))
                      (mergeloop (add1 left) right (add1 i)))
                    (begin
                      (vector-set! newvec i (vector-ref vec right))
                      (mergeloop left (add1 right) (add1 i)))))
               ((< left top-left)
                (vector-set! newvec i (vector-ref vec left))
                (mergeloop (add1 left) right (add1 i)))
               ((< right top-right)
                (vector-set! newvec i (vector-ref vec right))
                (mergeloop left (add1 right) (add1 i)))))))
        (mergeloop left right left)))))

; - End Program -

; - Program 10.11, pg. 314 -

(define vector-mergesort!
  (lambda (vec1)
    (let ((vec-size (vector-length vec1)))
      (let ((adjust (lambda (k) (min k vec-size)))
            (vec2 (make-vector vec-size))
            (max-index (sub1 vec-size)))
        (letrec 
          ((merge-pass
             (lambda (group-size count)
               (if (> group-size max-index)
                   (if (even? count) (vector-change! vec1 0 max-index vec2))
                   (let ((newvec (if (odd? count) vec2 vec1))
                         (vec (if (odd? count) vec1 vec2)))
                     (let ((merge! (vector-merge! newvec vec)))
                       (letrec 
                         ((group-ends
                            (lambda (left top-left right top-right)
                              (if (<= left max-index)
                                  (begin
                                    (merge! left top-left right top-right)
                                    (let ((new-right (+ top-right group-size)))
                                      (group-ends 
                                        top-right 
                                        (adjust new-right)
                                        new-right
                                        (adjust
                                          (+ new-right group-size)))))))))
                         (group-ends 0 (adjust group-size) 
                           group-size (adjust (* 2 group-size)))))
                     (merge-pass (* group-size 2) (add1 count)))))))
          (merge-pass 1 1))))))

; - End Program -

; - Program 10.12, pg. 315 -

(define vector-change!
  (lambda (vec1 j k vec2)
    (letrec ((loop (lambda (i)
                     (if (<= i k)
                         (begin
                           (vector-set! vec1 i (vector-ref vec2 i))
                           (loop (add1 i)))))))
      (loop j))))

; - End Program -

; - Program 10.13, pg. 317 -

(define quicksort
  (letrec
    ((collect
       (lambda (pivot ls lgroup rgroup)
         (if (null? ls)
             (append (quicksort lgroup) (cons pivot (quicksort rgroup)))
             (if (< pivot (car ls))
                 (collect pivot (cdr ls) lgroup (cons (car ls) rgroup))
                 (collect pivot (cdr ls) (cons (car ls) lgroup) rgroup))))))
    (lambda (ls)
      (if (or (null? ls) (null? (cdr ls)))
          ls
          (collect (car ls) (cdr ls) '() '())))))

; - End Program -

; - Program 10.14, pg. 320 -

(define vector-quicksort!
  (lambda (v)
    (letrec 
      ((qsort (lambda (low high)
                (if (< low high)
                    (let ((middle (partition v low high)))
                      (qsort low (sub1 middle))
                      (qsort (add1 middle) high))))))
      (qsort 0 (sub1 (vector-length v))))))

; - End Program -

; - Program 10.15, pg. 321 -

(define partition
  (lambda (v low high)
    (let ((pivot (vector-ref v low)))
      (letrec 
        ((search
           (lambda (left right)
             (letrec 
               ((search-up
                  (lambda (i)
                    (cond
                      ((= i (add1 right)) (sub1 i))
                      ((> (vector-ref v i) pivot) i)
                      (else (search-up (add1 i))))))
                (search-down 
                  (lambda (i)
                    (cond
                      ((or (= i (sub1 left)) (< (vector-ref v i) pivot)) i)
                      (else (search-down (sub1 i)))))))
               (let ((new-left (search-up left))
                     (new-right (search-down right)))
                 (if (< new-left new-right)
                     (begin
                       (vector-swap! v new-left new-right)
                       (search (add1 new-left) (sub1 new-right)))
                     (begin
                       (vector-swap! v low new-right)
                       new-right)))))))
        (search (add1 low) high)))))

; - End Program -

; - Program 10.16, pg. 321 -

(define vector-swap!
  (lambda (vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))))

; - End Program -

; - Program 10.18, pg. 324 -

(define random-list
  (lambda (n)
    (letrec ((build-list
               (lambda (k)
                 (if (zero? k)
                     '()
                     (cons (random n) (random-list (sub1 n)))))))
      (build-list n))))

; - End Program -

; - Program 10.19, pg. 325 -

; - If you are using PC-Scheme, replace (time-of-day) by (runtime). -
; - If you are using MacScheme, replace (time-of-day) by (time). -
; - If you are using Chez Scheme, replace (time-of-day) by (real-time)
      to get the actual elapsed time, or (cpu-time) for the cpu elapsed
      time. -
; - Most other Scheme systems have some timing mechanism that can be 
      used in place of (time-of-day). -

(define timer
  (lambda (proc arg)
    (let ((start (time-of-day)))
      (let ((val (proc arg)))
        (let ((finish (time-of-day)))
          (let ((elapsed-time (/ (- finish start) 100)))
            (writeln "Time = " elapsed-time ", Answer = " val)))))))

; - End Program -

; - Program 10.22, pg. 331 -

(define binary-search
  (lambda (rel?)
    (lambda (vec target)
      (letrec
        ((search 
           (lambda (left right)
             (if (< right left)
                 (writeln "The search failed.")
                 (let ((middle (floor (/ (+ left right) 2))))
                   (let ((mid-val (vector-ref vec middle)))
                     (cond 
                       ((rel? target mid-val) 
                        (search left (sub1 middle)))
                       ((rel? mid-val target) 
                        (search (add1 middle) right))
                       (else middle))))))))
         (search 0 (sub1 (vector-length vec)))))))

; - End Program -

; - Program 10.23, pg. 333 -

(define unlist
  (lambda (proc)
    (lambda (ls)
      (apply proc ls))))

; - End Program -

; - Program 10.25, pg. 337 -

(define find-supervisor
  (unlist
    (lambda (name id age yr-emp supervisor salary)
      (lambda (v) (if (string=? name v) supervisor #f)))))

; - End Program -

; - Program 10.26, pg. 338 -

(define closest-common-supervisor
  (letrec
    ((find-ccs
       (lambda (path1 path2)
         (let ((rest1 (cdr path1)) (rest2 (cdr path2)))
           (if (string=? (car rest1) (car rest2))
               (find-ccs rest1 rest2)
               (car path1))))))
    (lambda (test-procedure) 
      (lambda (table)
        (letrec 
          ((build-path
             (lambda (tbl u)
               (if (empty-set? tbl)
                   (list u)
                   (let ((next (pick tbl)))
                     (let ((v ((test-procedure next) u)))
                       (if (not v)
                           (build-path ((residue next) tbl) u)
                           (cons u (build-path table v)))))))))
          (lambda (x y)
            (find-ccs 
              (reverse (build-path table x))
              (reverse (build-path table y)))))))))

; - End Program -


