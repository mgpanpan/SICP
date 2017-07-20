#lang racket

(define tolerance 0.00001)
(define (fixed_point f first_guess)
  (define (close_enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (let ((next (f guess)))
      (display n)
      (display " ")
      (display guess)
      (newline)
      (if (close_enough? guess next)
          next
          (try next (+ n 1)))))
  (try first_guess 1))

(fixed_point cos 1.0)

(fixed_point (lambda (y) (+ (sin y) (cos y))) 1.0)

; 下面的这种求平方根值的方法是不收敛的
;(define (sqrt x)
;(fixed_point (lambda (y) (/ x y)) 1.0))

(define (sqrt x)
  (fixed_point (lambda (y) (average y (/ x y))) 1.0))
(define (average x y) (/ (+ x y) 2))

(sqrt 2)

; Exercise 1.35 golden ratio
(fixed_point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; Exercise 1.36 
(define (log_x x)
  (/ (log 1000) (log x)))

(fixed_point log_x 2.0)

(define (aver_damp f)
  (lambda (x) (/ (+ (f x) x) 2)))

(define log_x_aver_damp
  (aver_damp log_x))

(fixed_point log_x_aver_damp 2.0)

(define (square x)
        (* x x))

((aver_damp square) 10)
