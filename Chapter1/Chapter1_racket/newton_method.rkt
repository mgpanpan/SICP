#lang racket

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed_point (newton-transform g) guess))

(define tolerance 0.00001)
(define (fixed_point f first_guess)
  (define (close_enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close_enough? guess next)
          next
          (try next))))
  (try first_guess))

(define (square x) (* x x))

;(newton-method (lambda (x) (- (square x) 2.0))
;               1.0)

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newton-method (cubic 2.0 1.0 0) -3.0)
