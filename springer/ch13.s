; - Program 13.1, pg. 426 -

(define unif-rand-var-0-1
  (let ((big 1000000))
    (lambda ()
      (/ (+ 1 (random big)) big))))

; - End Program -

; - Program 13.2, pg. 427 -

(define exponential-random-variable
  (lambda (mean)
    (* mean (- (log (unif-rand-var-0-1))))))

; - End Program -

; - Program 13.3, pg. 427 -

(define arrival-time-generator
  (lambda (av-arr-time)
    (+ 1 (round (exponential-random-variable (- av-arr-time 1))))))

; - End Program -

; - Program 13.4, pg. 428 -

(define normal-random-variable
  (lambda (mean std-dev)
    (letrec ((compute (lambda (i)
                        (if (zero? i)
                            0
                            (+ (- (unif-rand-var-0-1) .5) 
                               (compute (sub1 i)))))))
      (+ mean (* std-dev (compute 12))))))

; - End Program -

; - Program 13.5, pg. 428 -

(define gallons-generator
  (lambda ()
    (max 1 (round (normal-random-variable 12 4)))))

; - End Program -

; - Exercise 13.3, pg. 429 -

(define random-maker
  (lambda (m a seed)
    (lambda (n)
      (let ((u (/ seed m)))
        (set! seed (modulo (* a seed) m))
        (floor (* n u))))))

(define random-time (lambda () 1000))

(define random 
  (random-maker (- (expt 2 32) 1) (expt 7 5) (random-time)))

; - End Exercise -

; - Program 13.8, pg. 434 -

(define simulation-setup&run
  (lambda (close-time %-self-service av-arr-time 
            profit-self profit-full 
            av-time-at-self-pump av-time-at-full-pump pump-rate)
    (let ((self-service (service-maker "Self" profit-self))
          (full-service (service-maker "Full" profit-full)))    
      (simulation
        (station-maker
          %-self-service
          self-service
          full-service
          av-time-at-self-pump
          av-time-at-full-pump
          pump-rate)
        (counter-maker 0 add1)
        av-arr-time
        (* 60 close-time)))))

; - End Program -

; - Program 13.9, pg. 435 -

(define simulation
  (lambda (station clock av-arr-time close-time)
    (let ((arrival 
            (box-maker (+ (send clock 'show) 
                          (arrival-time-generator av-arr-time)))))
      (letrec
        ((loop
           (lambda ()
             (if (= (send clock 'show) close-time)
                 (prepare-for-closing)
                 (begin
                   (if (= (send clock 'show) (send arrival 'show))
                       (begin
                         (send station 'which-serve
                           (customer-maker (send arrival 'show) clock))
                         (send station 'serve)
                         (send clock 'update!)
                         (send arrival 'update!
                           (+ (send clock 'show) 
                              (arrival-time-generator av-arr-time))))
                       (begin
                         (send station 'serve)
                         (send clock 'update!)))
                   (loop)))))
         (prepare-for-closing
           (lambda ()
             (if (send station 'all-empty?)
                 (send station 'report)
                 (begin
                   (send station 'serve)
                   (send clock 'update!)
                   (prepare-for-closing))))))
        (loop)))))

; - End Program -

; - Program 13.10, pg. 436 -

(define station-maker
  (let ((check (lambda (p) (send p 'check)))
        (all-empty? (andmap-c (lambda (p) (send p 'empty?))))
        (shorter (lambda (p1 p2)
                   (if (< (send p1 'size) (send p2 'size)) p1 p2))))
    (lambda (%-self self-serv full-serv av-time-self av-time-full pump-rate)
      (let ((selfs (list (pump-maker av-time-self pump-rate self-serv)
                         (pump-maker av-time-self pump-rate self-serv)))
            (fulls (list (pump-maker av-time-full pump-rate full-serv)
                         (pump-maker av-time-full pump-rate full-serv))))
        (lambda msg 
          (case (1st msg)
            ((type) "station")
            ((report) (send self-serv 'report) (send full-serv 'report))
            ((which-serve) 
             (let ((pump (apply shorter (if (< (random 100) %-self)
                                            selfs
                                            fulls))))
               (send pump 'enqueue! (2nd msg))))
            ((all-empty?) (and (all-empty? selfs) (all-empty? fulls)))
            ((serve) (for-each check selfs) (for-each check fulls))
            (else (delegate base-object msg))))))))

; - End Program -

; - Program 13.11, pg. 437 -

(define pump-maker
  (lambda (av-time pump-rate service)
    (let ((q (queue-maker)))
      (let ((increment (lambda ()
                         (let ((gallons (send (send q 'front) 'gallons)))
                           (ceiling (+ av-time (/ gallons pump-rate))))))
            (timer (box-maker -1)))
        (lambda msg
          (case (1st msg)
            ((type) "pump")
            ((check) (if (not (send q 'empty?))
                         (let ((c (send timer 'show)))
                           (cond
                             ((negative? c) (send timer 'update! (increment)))
                             ((zero? c)
                              (let ((customer (send q 'front)))
                                (send q 'dequeue!)
                                (send customer 'record service)
                                (if (send q 'empty?)
                                    (send timer 'reset!)
                                    (send timer 'update! (increment)))))
                             (else (send timer 'update!
                                     (sub1 (send timer 'show))))))))
            (else (delegate q msg))))))))

; - End Program -

; - Program 13.12, pg. 438 -

(define customer-maker  
  (lambda (arrival-time clock)
    (let ((gallons-pumped (gallons-generator)))
      (lambda msg
        (case (1st msg)
          ((type) "customer")
          ((gallons) gallons-pumped)
          ((record) (let ((service (2nd msg))
                          (wait (- (send clock 'show) arrival-time)))
                      (send service 'number-of!)
                      (send service 'total-wait! wait)
                      (send service 'max-wait! wait)
                      (send service 'total-profit! gallons-pumped)))
          (else (delegate base-object msg)))))))

; - End Program -

; - Program 13.13, pg. 439 -

(define service-maker
  (lambda (full-or-self profit)
    (let ((number-of (counter-maker 0 add1))
          (total-wait (accumulator-maker 0 +))
          (max-wait (accumulator-maker 0 max))
          (total-profit (accumulator-maker 0 +)))
      (lambda msg
          (case (1st msg)
            ((type) "service")
            ((number-of!) (send number-of 'update!))
            ((total-wait!) (send total-wait 'update! (2nd msg)))
            ((max-wait!) (send max-wait 'update! (2nd msg)))
            ((total-profit!) 
             (send total-profit 'update! (* profit (2nd msg))))
            ((report) (for-effect-only
                          (report full-or-self
                                  (send number-of 'show)
                                  (send total-wait 'show)
                                  (send max-wait 'show)
                                  (send total-profit 'show))))
            (else (delegate base-object msg)))))))

; - End Program -

; - Program 13.14, pg. 439 -

(define report
  (lambda (full-or-self num-cust total-wait max-wait profit)
    (if (zero? num-cust)
        (writeln "   There were no " full-or-self "-Service customers.")
        (begin
          (writeln full-or-self "-Service:")
          (writeln "   The number of customers is " num-cust)
          (writeln "   The average wait is " (round (/ total-wait num-cust)))
          (writeln "   The maximum wait is " max-wait)
          (writeln "   The total profit is " profit)))))

; - End Program -

; - Program 13.15, pg. 440 -

(define prompt-read
  (lambda (prompt)
    (display prompt)
    (display " ")
    (read)))

; - End Program -

; - Program 13.16, pg. 441 -

(define gas-station-simulator
  (letrec 
    ((loop (lambda (ls)
             (if (null? ls)
                 '()
                 (let ((v (prompt-read (car ls))))
                   (cons v (loop (cdr ls))))))))
    (lambda ()
      (apply simulation-setup&run (loop station-prompts)))))

; - End Program -

; - Exercise 13.9, pg. 444 -

(define prompt-read
  (lambda items
    (for-each display items)
    (display " ")
    (read)))

; - End Exercise -

