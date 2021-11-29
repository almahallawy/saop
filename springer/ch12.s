; - Program 12.1, pg. 386 -

(define for-effect-only
  (lambda (item-ignored)
    "unspecified value"))

; - End Program -

; - Program 12.2, pg. 388 -

(define box-maker
  (lambda (init-value)
    (let ((contents init-value))
      (lambda msg
        (case (1st msg)
          ((type) "box")
          ((show) contents)
          ((update!) (for-effect-only (set! contents (2nd msg))))
          ((swap!) (let ((ans contents)) 
                     (set! contents (2nd msg)) 
                     ans))
          ((reset!) (for-effect-only (set! contents init-value)))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.3, pg. 388 -

(define delegate
  (lambda (obj msg)
    (apply obj msg)))

; - End Program -

; - Program 12.4, pg. 389 -

(define base-object
  (lambda msg
    (case (1st msg)
      ((type) "base-object")
      (else invalid-method-name-indicator))))

; - End Program -

; - Program 12.5, pg. 389 -

(define send
  (lambda args
    (let ((object (car args)) (message (cdr args)))
      (let ((try (apply object message)))
        (if (eq? invalid-method-name-indicator try)
            (error 
              (string-append (symbol->string (car message)) ": "
                             "Bad method name sent to object of " 
                             (object 'type) " type."))
            try)))))

; - End Program -

; - Program 12.6, pg. 390 -

(define box-maker
  (lambda (init-value)
    (let ((cell (cons init-value "any value")))
      (lambda msg
        (case (1st msg)
          ((type) "box")
          ((update!) (for-effect-only (set-car! cell (2nd msg))))
          ((swap!) (let ((ans (car cell)))
                     (set-car! cell (2nd msg))
                     ans))
          ((show) (car cell))
          ((reset!) (for-effect-only (set-car! cell init-value)))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.7, pg. 391 -

(define counter-maker
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "counter")
          ((update!) (let ((result (unary-proc (send total 'show))))
                       (send total 'update! result)))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))

; - End Program -

; - Program 12.8, pg. 392 -

(define counter-maker
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "counter")
          ((update!) (send total 'update! 
                       (unary-proc (send total 'show))))
          ((show reset) (delegate total msg))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.9, pg. 393 -

(define accumulator-maker
  (lambda (init-value binary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "accumulator")
          ((update!) 
           (send total 'update! 
             (binary-proc (send total 'show) (2nd msg))))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))

; - End Program -

; - Program 12.10, pg. 394 -

(define gauge-maker
  (lambda (init-value unary-proc-up unary-proc-down)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "gauge")
          ((up!) (send total 'update! 
                   (unary-proc-up (send total 'show))))
          ((down!) (send total 'update! 
                     (unary-proc-down (send total 'show))))
          ((swap! update!) (delegate base-object msg))
          (else (delegate total msg)))))))

; - End Program -

; - Program 12.12, pg. 398 -

(define stack-maker
  (lambda ()
    (let ((stk '()))
      (lambda msg
        (case (1st msg)
          ((type) "stack")
          ((empty?) (null? stk))
          ((push!) (for-effect-only 
                     (set! stk (cons (2nd msg) stk))))
          ((top) (if (null? stk)
                     (error "top: The stack is empty.")
                     (car stk)))
          ((pop!) (for-effect-only
                    (if (null? stk)
                        (error "pop!: The stack is empty.")
                        (set! stk (cdr stk)))))
          ((size) (length stk))
          ((print) (display "TOP: ")
                   (for-each 
                     (lambda (x) 
                       (display x)
                       (display " "))
                     stk)
                   (newline))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.13, pg. 400 -

(define queue-maker
  (lambda ()
    (let ((q '()))
      (lambda msg
        (case (1st msg)
          ((type) "queue")
          ((empty?) (null? q))
          ((enqueue!) (for-effect-only
                        (let ((list-of-item (cons (2nd msg) '())))
                          (if (null? q)
                              (set! q list-of-item)
                              (append! q list-of-item)))))
          ((front) (if (null? q)
                       (error "front: The queue is empty.")
                       (car q)))
          ((dequeue!) (for-effect-only 
                        (if (null? q)
                            (error "dequeue!: The queue is empty.")
                            (set! q (cdr q)))))
          ((size) (length q))
          ((print) (display "FRONT: ")
                   (for-each 
                     (lambda (x) (display x) (display " ")) 
                     q)
                   (newline))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.15, pg. 402 -

(define queue-maker
  (lambda ()
    (let ((q '()) (rear "any value"))
      (lambda msg
        (case (1st msg)
          ((type) "queue")
          ((empty?) (null? q))
          ((enqueue!) (for-effect-only
                        (let ((list-of-item (cons (2nd msg) '())))
                          (if (null? q)
                              (begin
                                (set! rear list-of-item)
                                (set! q list-of-item))
                              (begin
                                (set-cdr! rear list-of-item)
                                (set! rear list-of-item))))))
          ((front) (if (null? q)
                       (error "front: The queue is empty.")
                       (car q)))
          ((dequeue!) (for-effect-only
                        (if (null? q)
                            (error "dequeue!: The queue is empty.")
                            (begin
                              (set! q (cdr q))
                              (if (null? q) 
                                  (set! rear 'anything))))))
          ((size) (length q))
          ((print) (display "FRONT: ")
                   (for-each 
                     (lambda (x) (display x) (display " ")) q)
                   (newline))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.18, pg. 406 -

(define circular-list-maker
  (lambda ()
    (let ((marker '())
          (size-gauge (gauge-maker 0 add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "circular list")
          ((empty?) (null? marker))          
          ((insert!) (send size-gauge 'up!)
                     (for-effect-only
                       (if (null? marker)
                           (begin
                             (set! marker (cons (2nd msg) '()))
                             (set-cdr! marker marker))
                           (set-cdr! marker (cons (2nd msg) (cdr marker))))))
          ((head) (if (null? marker)
                      (error "head: No last entry in an empty list.")
                      (car (cdr marker))))
          ((delete!) (send size-gauge 'down!)
                     (for-effect-only
                       (if (null? marker)
                           (error "delete!: The circular list is empty.")
                           (if (eq? marker (cdr marker))
                               (set! marker '())
                               (set-cdr! marker (cdr (cdr marker)))))))
          ((move!) (for-effect-only
                     (if (null? marker)
                         (error "move!: The circular list is empty.") 
                         (set! marker (cdr marker)))))
          ((size) (send size-gauge 'show))
          ((print) (for-effect-only
                     (if (null? marker)
                         (display "")
                         (let ((next (cdr marker)))
                           (set-cdr! marker '())
                           (for-each (lambda (x) (display x) (display " ")) 
                             next)
                           (set-cdr! marker next)))))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.19, pg. 407 -

(define stack-maker
  (lambda ()
    (let ((c (circular-list-maker)))
      (lambda msg
        (case (1st msg)
          ((type) "stack")
          ((push!) (send c 'insert! (2nd msg)))
          ((pop!) (send c 'delete!))
          ((top) (send c 'head))
          ((print) (display "TOP: ") (send c 'print))
          ((insert! head delete! move!) (delegate base-object msg))
          (else (delegate c msg)))))))

; - End Program -

; - Program 12.20, pg. 408 -

(define queue-maker
  (lambda ()
    (let ((c (circular-list-maker)))
      (lambda msg
        (case (1st msg)
          ((type) "queue")
          ((enqueue!) (send c 'insert! (2nd msg)) (send c 'move!))
          ((dequeue!) (send c 'delete!))
          ((front) (send c 'head))
          ((print) (display "FRONT: ") (send c 'print))
          ((insert! head delete! move!) (delegate base-object msg))
          (else (delegate c msg)))))))          

; - End Program -

; - Exercise 12.21, pg. 411 -

(define memoize
  (lambda (proc)
    (let ((bucket (bucket-maker)))
      (lambda (arg)
        (send bucket 'update!-lookup arg (lambda (val) val) proc)))))

; - End Exercise -

; - Program 12.23, pg. 412 -

(define bucket-maker
  (lambda ()
    (let ((table '()))
      (lambda msg
        (case (1st msg)
          ((type) "bucket")
          ((lookup)
           (let ((key (2nd msg)) (succ (3rd msg)) (fail (4th msg)))
             (lookup key table (lambda (pr) (succ (cdr pr))) fail)))
          ((update!)
           (for-effect-only
             (let ((key (2nd msg)) 
                   (updater (3rd msg)) 
                   (initializer (4th msg)))
               (lookup key table
                 (lambda (pr)
                   (set-cdr! pr (updater (cdr pr))))
                 (lambda ()
                   (let ((pr (cons key (initializer key))))
                     (set! table (cons pr table))))))))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 12.24, pg. 412 -

(define memoize
  (lambda (proc)
    (let ((bucket (bucket-maker)))
      (lambda (arg)
        (send bucket 'update! arg (lambda (val) val) proc)
        (send bucket 'lookup arg 
          (lambda (val) val) (lambda () #f))))))

; - End Program -

; - Program 12.25, pg. 414 -

(define hash-table-maker
  (lambda (size hash-fn)
    (let ((v ((vector-generator (lambda (i) (bucket-maker))) size)))
      (lambda msg
        (case (1st msg)
          ((type) "hash table")
          (else
            (delegate (vector-ref v (hash-fn (2nd msg))) msg)))))))

; - End Program -

; - Program 12.26, pg. 415 -

(define memoize
  (lambda (proc)
    (let ((hashf (lambda (x) (remainder x 1000))))
      (let ((h (hash-table-maker 1000 hashf)))
        (lambda (arg)
          (send h 'update! arg (lambda (v) v) proc)
          (send h 'lookup arg (lambda (v) v) (lambda () #f)))))))

; - End Program -

; - Exercise 12.29, pg. 418 -

(define theater-maker
  (lambda (capacity)
    (let ((ticket-line (queue-maker))
          (vacancies (gauge-maker capacity add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "theater")
          ((dequeue!) (if (zero? (send vacancies 'show))
                          (display "doors closed")
                          (begin
                            (send ticket-line 'dequeue!)
                            (send vacancies 'down!))))
          ((leave!) (if (< (send vacancies 'show) capacity)
                        (send vacancies 'up!)
                        (error "leave!: The theater is empty.")))
          (else (delegate ticket-line msg)))))))

; - End Exercise -

; - Exercise 12.30, pg. 418 -

(define theater-maker
  (lambda (capacity)
    (let ((ticket-line (queue-maker))
          (vacancies (gauge-maker capacity add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "theater")
          ((dequeue!) (if (zero? (send vacancies 'show))
                          (display "doors closed")
                          (begin
                            (send ticket-line 'dequeue!)
                            (send vacancies 'down!))))
          ((leave!) (if (< (send vacancies 'show) capacity)
                        (send vacancies 'up!)
                        (error "leave!: The theater is empty.")))
          ((show) (send vacancies 'show))
          (else (delegate ticket-line msg)))))))

; - End Exercise -

; - Program 12.27, pg. 419 -

(define combine
  (lambda (f g)
    (lambda msg
      (let ((f-try (delegate f msg)))
        (if (eq? invalid-method-name-indicator f-try)
            (delegate g msg)
            f-try)))))

; - End Program -

; - Exercise 12.30, pg. 418 -

(define theater-maker
  (lambda (capacity)
    (let ((ticket-line (queue-maker))
          (vacancies (gauge-maker capacity add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "theater")
          ((dequeue!) (if (zero? (send vacancies 'show))
                          (display "doors closed")
                          (begin
                            (send ticket-line 'dequeue!)
                            (send vacancies 'down!))))
          ((leave!) (if (< (send vacancies 'show) capacity)
                        (send vacancies 'up!)
                        (error "leave! The theater is empty.")))
          (else (delegate (combine ticket-line vacancies) msg)))))))

; - End Exercise -

; - Exercise 12.32, pg. 420 -

(define theater-maker
  (lambda (capacity)
    (let ((ticket-line (queue-maker))
          (vacancies (gauge-maker capacity add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "theater")
          ((dequeue!) (if (zero? (send vacancies 'show))
                          (display "doors closed")
                          (begin
                            (send ticket-line 'dequeue!)
                            (send vacancies 'down!))))
          ((leave!) (if (< (send vacancies 'show) capacity)
                        (send vacancies 'up!)
                        (error "leave!: The theater is empty.")))
          ((reset! update!) (delegate base-object msg))
          (else (delegate (combine ticket-line vacancies) msg)))))))

; - End Exercise -

; - Program 12.28, pg. 421 -

(define send
  (lambda args
    (let ((try (apply (car args) args)))
      (if (eq? invalid-method-name-indicator try)
          (let ((object (car args)) (message (cdr args)))
            (error
              (string-append (symbol->string (car message)) ": "
                "Bad method name sent to object of " 
                (object object 'type) " type.")))
          try))))

; - End Program -

; - Exercise 12.33, pg. 421 -

(define counter-maker
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda message
        (let ((self (car message)) (msg (cdr message)))
          (case (1st msg)
            ((type) "counter")
            ((update!) (let ((result (unary-proc (send total 'show))))
                         (send total 'update! result)))
            ((swap!) (delegate base-object message))
            (else (delegate total message))))))))

; - End Exercise -

; - Exercise 12.34, pg. 422 -

(define cartesian-point-maker 
  (lambda (x-coord y-coord) 
    (lambda message
      (let ((self (car message)) (msg (cdr message))) 
        (case (1st msg)
          ((type) "Cartesian point")
          ((distance) (sqrt (+ (square x-coord) (square y-coord))))          
          ((closer?) (< (send self 'distance) (send (2nd msg) 'distance)))
          (else (delegate base-object message)))))))

; - End Exercise -

; - Exercise 12.35, pg. 423 -

(define manhattan-point-maker
  (lambda (x-coord y-coord)
    (let ((p (cartesian-point-maker x-coord y-coord)))
      (lambda message
        (let ((self (car message)) (msg (cdr message)))
          (case (1st msg)
            ((type) "Manhattan point")
            ((distance) (+ x-coord y-coord))
            (else (delegate p message))))))))

; - End Exercise -

; - Exercise 12.36, pg. 423 -

(define cartesian-origin-maker
  (lambda ()
    (let ((x-coord 0) (y-coord 0))
      (lambda message
        (let ((self (car message)) (msg (cdr message)))
          (case (car msg)
            ((type) "Cartesian point")
            ((distance) (sqrt (+ (square x-coord) (square y-coord))))
            ((closer?) (< (send self 'distance) (send (2nd msg) 'distance)))
            (else (delegate base-object message))))))))

; - End Exercise -


