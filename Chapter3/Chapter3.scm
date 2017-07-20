;; section 3.1, Assignment and Local State
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)
balance

;; make balance internal to withdraw
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 15)

;; -------------------------------------------------------------------
;; below 2 are equivalent to each other, but neither works as desired.
(define (new-withdraw amount)
  (let ((balance 100))
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 15)

(define new-withdraw
  (lambda (amount)
    (let ((balance 100))
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 15)
;; -------------------------------------------------------------------

;; make-withdraw, creates "withdrawal processors"
;; the formal parameter balance in make-withdraw specifies the initial
;; amount of money in the account.
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))
(w1 50)
(w2 70)
(w2 40)
(w1 40)
((make-withdraw 100) 99)
((make-withdraw 100) 98)

;; create objects that handle deposits as well as withdrawals
;; use message-passing style of programming
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 100)
((acc 'withdraw) 0.1)
((acc 'deposit) 99)
((acc 'deposit) 1.1)
((acc 'withdraw) 10)

;; 3.1.2

;; 3.1.3
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))
((peter-acc 'withdraw) 10)
((paul-acc 'withdraw) 50)
((peter-acc 'withdraw) 30)
((paul-acc 'withdraw) 30)

(define peter-acc (make-account 100))
(define paul-acc peter-acc)
((peter-acc 'withdraw) 10)
((paul-acc 'withdraw) 50)
((peter-acc 'withdraw) 30)
((paul-acc 'withdraw) 30)

;; section 3.3
(define x '((a b) c d))
(define y '(e f))
(set-car! x y)
(set-cdr! x y)

(cons 'a 'b)
(cons x y)
;; get-new-pair is not primitive.
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))
(cons 'a 'b)

;; test
(define x '(a b c))
(define (change-cdr x)
  (set-cdr! x '(d e)))
(change-cdr x)

;; sharing and identity
 (define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow))
(set-to-wow! z1)
(set-to-wow! z2)

;; 3.3.2
;; queue
;; internal function
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

;; 
(define (make-queue) (cons '() '()))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue! queue)
         (error "DELETE! called with an empty queue"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue))))))

;; 3.3.3 Representing Tables
(define (make-table)
  (list '*table*))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else
         (assoc key (cdr records)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value)
                              (cdr table)))))
  'ok)
(define (print-table table)
  (define (print-key-value records)
    (cond ((null? records)
           'done)
          (else
           (display (caar records))
           (display " : ")
           (display (cdar records))
           (newline)
           (print-key-value (cdr records))
           )))
  (display (car table))
  (newline)
  (print-key-value (cdr table)))

(define t1 (make-table))
(insert! 'a 1 t1)
(insert! 'b 2 t1)
(insert! 'c 3 t1)
(print-table t1)
(lookup 'a t1)
(lookup 'b t1)
(lookup 'c t1)
(lookup 'd t1)

;; two-dimensional tables
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((records (assoc key-2 (cdr subtable))))
          (if records
              (cdr record)
              false))
        false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value)
                                       (cdr subtable)))))
        (set-cdr! table (cons (list key-1
                                    (cons key-2 value))
                              (cdr table)))))
  'ok)

(define t2 (make-table))
(insert! 'math '+ 43 t2)
(insert! 'letters 'a 97 t2)
(insert! 'math '- 45 t2)
(insert! 'math '* 42 t2)
(insert! 'letters 'b 98 t2)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((records (assoc key-2 (cdr subtable))))
              (if records
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! table (cons (list key-1
                                        (cons key-2 value))
                                  (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 3.3.4 A Simulator for Digital Circuits
;; wire operation
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action) action-procedure))

;; primitive functions in digital logic simulator
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
        ((and (= a1 0) (= a2 1)) 0)
        ((and (= a1 1) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "Invalid signal" a1 a2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
        ((and (= a1 0) (= a2 1)) 1)
        ((and (= a1 1) (= a2 0)) 1)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "Invalid signal" a1 a2))))

;; the agenda
;; agenda is made up of time segments
(define (make-time-segment time queue)
  (cons times queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
