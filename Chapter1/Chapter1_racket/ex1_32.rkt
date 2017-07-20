#lang racket

; recursion
;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (term a)
;                (accumulate combiner null-value term (next a) next b))))

; iteration
(define (accumulate combiner null-value term a next b)
  (define (accumulate_iter a result)
    (if (> a b)
        result
        (accumulate_iter (next a) (combiner result (term a)))))
  (accumulate_iter a null-value))

; sum
; 将sum看成是accumulate的一个特例
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (sum-integer a b)
  (sum identity a inc b))
(sum-integer 1 10)
(define (cubes x) (* x x x))
(define (sum-cubes a b)
  (sum cubes a inc b))
(sum-cubes 1 10)
(define (pi-term x)
  (/ 1.0 x (+ x 2)))
(define (pi-next x) (+ x 4))
(define (pi-sum a b)
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))
(integral cubes 0 1 0.001)

; product
; 将product看成是accumulate的一个特例
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-integer a b)
  (product identity a inc b))
(product-integer 1 10)

(define (term_pi x)
  (* (/ (- x 1.0) x) (/ (+ x 1.0) x)))
(define (next_pi x)
  (+ x 2))
(define (pi_appro n)
  (* 4.0 (product term_pi 3 next_pi n)))
(pi_appro 10000)
