; - Program 9.1, pg. 269 -

(define view 
  (lambda (vec) 
    (let ((highest-index (sub1 (vector-length vec)))) 
      (letrec ((loop (lambda (i) 
                       (display (vector-ref vec i)) 
                       (if (< i highest-index) 
                           (begin
                             (display " ") 
                             (loop (add1 i))))))) 
        (display "#(") 
        (loop 0) 
        (display ")")))))

; - End Program -

; - Program 9.2, pg. 270 -

(define make-vector 
  (lambda args 
    (let ((fill-value 
            (if (singleton-list? args)
                '() 
                (cadr args)))) 
      ((vector-generator (lambda (i) fill-value)) (car args)))))

; - End Program -

; - Program 9.3, pg. 271 -

(define list->vector 
  (lambda (ls) 
    ((vector-generator (lambda (i) (list-ref ls i))) (length ls))))

; - End Program -

; - Program 9.4, pg. 271 -

(define vector 
  (lambda args
    (list->vector args)))

; - End Program -

; - Program 9.5, pg. 272 -

(define vector-stretch
  (lambda (vec new-size) 
    (let ((size (vector-length vec))) 
      (let ((gen-proc (lambda (i) 
                        (if (< i size) 
                            (vector-ref vec i) 
                            '()))))
        ((vector-generator gen-proc) new-size)))))

; - End Program -

; - Program 9.6, pg. 272 -

(define vector-copy
  (lambda (vec) 
    (vector-stretch vec (vector-length vec))))

; - End Program -

; - Program 9.7, pg. 272 -

(define vector-update
  (lambda (vec k val) 
    (let ((gen-proc (lambda (i) 
                      (if (= i k) 
                          val
                          (vector-ref vec i))))) 
      ((vector-generator gen-proc) (vector-length vec)))))

; - End Program -

; - Program 9.8, pg. 273 -

(define list->vector
  (lambda (ls) 
    (let ((vec (make-vector (length ls)))) 
      (letrec 
        ((convert (lambda (ls* v i) 
                    (if (null? ls*) 
                        v 
                        (let ((new-v (vector-update v i (car ls*))))
                          (convert (cdr ls*) new-v (add1 i))))))) 
        (convert ls vec 0)))))

; - End Program -

; - Program 9.9, pg. 273 -

(define vector-map
  (lambda (proc vec) 
    ((vector-generator (lambda (i) (proc (vector-ref vec i)))) 
     (vector-length vec))))

; - End Program -

; - Program 9.10, pg. 274 -

(define multiply-by-scalar 
  (lambda (c vec) 
    (vector-map (lambda (elem) (* c elem)) vec)))

; - End Program -

; - Program 9.11, pg. 274 -

(define vector-apply-elementwise-to-both 
  (lambda (proc) 
    (lambda (vec1 vec2) 
      (let ((gen-proc 
              (lambda (i) 
                (proc (vector-ref vec1 i) (vector-ref vec2 i))))) 
        ((vector-generator gen-proc) (vector-length vec1))))))

; - End Program -

; - Program 9.12, pg. 275 -

(define vec+ (vector-apply-elementwise-to-both +))

(define vec* (vector-apply-elementwise-to-both *)) 

; - End Program -

; - Program 9.13, pg. 275 -

(define vector-sum
  (lambda (vec) 
    (let ((size (vector-length vec))) 
      (letrec 
        ((helper
           (lambda (i) 
             (if (= i size) 
                 0 
                 (+ (vector-ref vec i) (helper (add1 i))))))) 
        (helper 0)))))

; - End Program -

; - Program 9.14, pg. 276 -

(define vector-product 
  (lambda (vec) 
    (let ((size (vector-length vec)))
      (letrec 
        ((helper 
           (lambda (i) 
             (if (= i size) 
                 1 
                 (* (vector-ref vec i) (helper (add1 i))))))) 
        (helper 0)))))

; - End Program -

; - Program 9.15, pg. 277 -

(define vector-accumulate 
  (lambda (proc seed) 
    (lambda (vec) 
      (let ((size (vector-length vec))) 
        (letrec 
          ((helper 
             (lambda (i) 
               (if (= i size)
                   seed 
                   (proc (vector-ref vec i) (helper (add1 i))))))) 
          (helper 0))))))

; - End Program -

; - Program 9.16, pg. 277 -

(define vector->list (vector-accumulate cons '()))

; - End Program -

; - Program 9.17, pg. 279 -

(define dot-product 
  (lambda (vec1 vec2) 
    (let ((size (vector-length vec1))) 
      (letrec 
        ((loop 
           (lambda (i acc) 
             (if (= i size) 
                 acc 
                 (loop (add1 i) 
                       (+ acc (* (vector-ref vec1 i) 
                                 (vector-ref vec2 i)))))))) 
        (loop 0 0)))))

; - End Program -

; - Program 9.18, pg. 279 -

(define vector?
  (lambda (arg)
    (and (pair? arg) (eq? (car arg) vector-tag))))

(define vector-length
  (lambda (vec)
    (car (cdr vec))))

; - End Program -

; - Program 9.19, pg. 280 -

(define vector-ref
  (lambda (vec i)
    ((cddr vec) i)))  

(define vector-generator
  (lambda (gen-proc)
    (lambda (size)
      (cons vector-tag (cons size gen-proc))))) 

; - End Program -

; - Program 9.20, pg. 280 -

(define vector-ref
  (lambda (vec i)
    (list-ref (cddr vec) i)))

(define vector-generator
  (lambda (gen-proc)
    (lambda (size)
      (cons vector-tag
            (cons size
                  (letrec
                    ((loop (lambda (i)
                             (cond
                               ((= i size) '())
                               (else (cons (gen-proc i) 
                                           (loop (add1 i))))))))
                    (loop 0)))))))  

; - End Program -

; - Program 9.21, pg. 283 -

(define vector-generator
  (lambda (gen-proc)
    (lambda (size)
      (let ((vec (make-vector size)))
        (letrec
          ((loop (lambda (i)
                   (if (< i size)
                       (begin
                         (vector-set! vec i (gen-proc i))
                         (loop (add1 i)))))))
          (loop 0))
        vec))))

; - End Program -

; - Program 9.22, pg. 283 -

(define vector-update!
  (lambda (vec i c) 
    (vector-set! vec i c) 
    vec))

; - End Program -

; - Program 9.23, pg. 284 -

(define list->vector
  (lambda (ls)
    (let ((vec (make-vector (length ls))))
      (letrec
        ((convert
           (lambda (ls i)
             (if (not (null? ls))
                 (begin
                   (vector-set! vec i (car ls))
                   (convert (cdr ls) (add1 i)))))))
        (convert ls 0))
      vec)))

; - End Program -

; - Program 9.24, pg. 286 -

(define vector-reverse 
  (lambda (vec) 
    (letrec 
      ((switch 
         (lambda (v i j)
           (if (>= i j) 
               v 
               (let ((swapv (swap-maker v))) 
                 (switch (swapv i j) (add1 i) (sub1 j))))))) 
      (switch vec 0 (sub1 (vector-length vec))))))

; - End Program -

; - Program 9.25, pg. 286 -

(define swap-maker 
  (lambda (vec) 
    (lambda (index1 index2) 
      (let ((temp (vector-ref vec index1))) 
        (vector-update 
          (vector-update vec index1 (vector-ref vec index2)) 
          index2 
          temp)))))

; - End Program -

; - Program 9.26, pg. 287 -

(define vector-reverse!
  (lambda (vec)
    (let ((swapv! (swap-maker vec)))
      (letrec 
        ((switch (lambda (i j)
                   (if (< i j)
                       (begin
                         (swapv! i j)
                         (switch (add1 i) (sub1 j)))))))
        (switch 0 (sub1 (vector-length vec))))
      vec)))

; - End Program -

; - Program 9.27, pg. 288 -

(define swap-maker
  (lambda (vec)
    (lambda (index1 index2)
      (let ((temp (vector-ref vec index1)))
        (vector-update!
          (vector-update! vec index1 (vector-ref vec index2))
          index2
          temp)))))

; - End Program -

; - Program 9.30, pg. 293 -

(define num-cols
  (lambda (mat)
    (let ((size (sub1 (vector-length mat))))
      (vector-ref mat size))))

; - End Program -

; - Program 9.31, pg. 294 -

(define num-rows
  (lambda (mat)
    (let ((size (sub1 (vector-length mat))))
      (/ size (vector-ref mat size)))))

; - End Program -

; - Program 9.32, pg. 294 -

(define matrix-ref
  (lambda (mat)
    (let ((ncols (num-cols mat)))
      (lambda (i j)
        (vector-ref mat (+ (* i ncols) j))))))

; - End Program -

; - Program 9.33, pg. 295 -

(define matrix-generator
  (lambda (gen-proc)
    (lambda (nrows ncols)
      (let ((size (* nrows ncols)))
        (let ((vec-gen-proc
                (lambda (k) 
                  (if (< k size)
                      (gen-proc (quotient k ncols) 
                                (remainder k ncols))
                      ncols))))
          ((vector-generator vec-gen-proc) 
           (add1 size)))))))

; - End Program -

; - Program 9.34, pg. 296 -

(define row-of 
  (lambda (mat)
    (let ((mat-ref (matrix-ref mat))
          (number-of-columns (num-cols mat)))
      (lambda (i)
        (let ((gen-proc (lambda (j) (mat-ref i j))))
          ((vector-generator gen-proc) number-of-columns))))))

; - End Program -

; - Program 9.35, pg. 296 -

(define column-of
  (lambda (mat)
    (let ((mat-ref (matrix-ref mat))
          (number-of-rows (num-rows mat)))
      (lambda (j)
        (let ((gen-proc (lambda (i) (mat-ref i j))))
          ((vector-generator gen-proc) number-of-rows))))))

; - End Program -

; - Program 9.37, pg. 297 -

(define matrix-transpose
  (lambda (mat)
    (let ((mat-ref (matrix-ref mat)))
      (let ((gen-proc (lambda (i j) (mat-ref j i))))
        ((matrix-generator gen-proc) 
         (num-cols mat) 
         (num-rows mat))))))

; - End Program -

; - Program 9.38, pg. 299 -

(define matrix-product
  (lambda (mat-a mat-b)
    (let ((ncols-a (num-cols mat-a))
          (a-ref (matrix-ref mat-a))
          (b-ref (matrix-ref mat-b)))
      (if (not (= ncols-a (num-rows mat-b)))
          (error "matrix-product:"
            "The matrices are not compatible.")
          (let 
            ((gen-proc 
               (lambda (i j)
                 (letrec 
                   ((loop 
                      (lambda (r acc)
                        (if (= r ncols-a)
                            acc
                            (loop (add1 r) 
                                  (+ acc (* (a-ref i r) 
                                            (b-ref r j))))))))
                   (loop 0 0)))))           
            ((matrix-generator gen-proc) 
             (num-rows mat-a) (num-cols mat-b)))))))

; - End Program -

; - Program 9.39, pg. 300 -

(define matrix-set!
  (lambda (mat)
    (let ((ncols (num-cols mat)))
      (lambda (i j obj)
        (vector-set! mat (+ (* i ncols) j) obj)))))

; - End Program -


