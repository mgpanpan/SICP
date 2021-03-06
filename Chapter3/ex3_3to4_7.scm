;; in the text written by the author
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

;; Exercise 3.3. add passward
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password m)
    (if (eq? input-password password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (x) "Incorrect passward")))
  dispatch)
(define acc (make-account 100 'emacs))
((acc 'guess 'withdraw) 40)
((acc 'emacs 'withdraw) 40)
((acc 'guess 'deposit) 1000)
((acc 'emacs 'deposit) 1000)
((acc 'emacs 'withdraw) 2000)
((acc 'emacs 'withdraw) 100)

;; Exercise 3.4. add another local state variable "times", if an account
;; is accessed more than 7 consecutive times with an incorrect passward,
;; it invokes the procedure call-the-cops
(define (make-account balance password)
  (let ((times 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      "-_-")
    (define (dispatch input-password m)
      (if (eq? input-password password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (lambda (x) (set! times (+ times 1))
                  (if (= times 3)
                      (call-the-cops)
                      "Incorrect password"))))
    dispatch))
(define acc (make-account 100 'emacs))
((acc 'guess 'withdraw) 40)
((acc 'emacs 'withdraw) 40)
((acc 'guess 'deposit) 1000)
((acc 'emacs 'deposit) 1000)
((acc 'emacs 'withdraw) 2000)
((acc 'emacs 'withdraw) 100)
((acc 'hello 'withdraw) 1000)
((acc 'emacs 'deposit) 100)

;; Exercise 3.7
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password m)
    (if (eq? input-password password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'set-new-password) (set! password ))
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (x) "Incorrect passward")))
  dispatch)

;; there are 3 times of checking password:
;; 1: when set up joint account, if password if incorrect, will not set up
;; the new joint account.
;; 2: when access joint account using the new-password
;; 3: when (acc password m)
(define (make-joint acc password new-password)
  (define (joint-dispatch input-password m)
    (if (eq? input-password new-password)
        (acc password m)
        (lambda (x) "Incorrect password")))
  (if (number? ((peter-acc password 'withdraw) 0));; when the password is correct, return number
      joint-dispatch
      (display "Incorrect password")
      ))
  
(define peter-acc (make-account 100 'emacs))
((peter-acc 'guess 'withdraw) 40)
((peter-acc 'emacs 'withdraw) 40)
((peter-acc 'guess 'deposit) 1000)
((peter-acc 'emacs 'deposit) 1000)
((peter-acc 'emacs 'withdraw) 2000)
((peter-acc 'emacs 'withdraw) 100)

(define paul-acc
  (make-joint peter-acc 'eeemacs 'rosebud))
(define paul-acc
  (make-joint peter-acc 'emacs 'rosebud))

((paul-acc 'eeeee 'withdraw) 40)
((paul-acc 'emacs 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 100)
((peter-acc 'rosebud 'withdraw) 500)
((peter-acc 'emacs 'withdraw) 500)
((peter-acc 'emacs 'deposit) 100)
((paul-acc 'rosebud 'deposit) 200)
((peter-acc 'emacs 'withdraw) 0)
((paul-acc 'rosebud 'withdraw) 0)
