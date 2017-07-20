#lang racket

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

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

(define (fixed_point_of_transform g transform guess)
  (fixed_point (transform g) guess))

;(define (sqrt x)
;  (fixed_point_of_transform (lambda (y) (- (square y) x))
;                            newton-transform
;                            1.0))

;(sqrt 2.0)

(define (aver_damp f)
  (lambda (x) (/ (+ (f x) x) 2)))

(define (sqrt x)
  (fixed_point_of_transform (lambda (y) (/ x y))
                            aver_damp
                            1.0))

(sqrt 2.0)
