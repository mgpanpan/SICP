#lang racket

; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)

; exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

((compose square inc) 6)

; exercise 1.43
(define (repeated f n)
  (define (repeated_iter i result)
    (if (> i n)
        result
        (repeated_iter (+ i 1) (compose f result))))
  (repeated_iter 1 (lambda (x) x)))

((repeated square 3) 5)

((repeated (lambda (x) (+ x 1)) 100) 5)

; exercise 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (smooth_ntimes f n)
  ((repeated smooth n) f))

; exercise 1.46
(define (iterative_improve good_enough improve_guess initial_value)
  (let ((next_value (improve_guess initial_value)))
    (if (good_enough initial_value next_value)
        next_value
        (iterative_improve good_enough improve_guess next_value))))

(define (good_enough initial_value next_value)
  (< (abs (- initial_value next_value)) 0.00001))

(define (sqrt x)
  (iterative_improve good_enough
                     (lambda (y) (/ (+ y (/ x y)) 2))
                     1.0))

(sqrt 2.0)

(define (fixed_point f first_guess)
  (iterative_improve good_enough
                     (lambda (y) (f y))
                     first_guess))

(fixed_point cos 1.0)

(fixed_point (lambda (y) (+ (sin y) (cos y))) 1.0)
