#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
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

; using lambda
(define (integral f a b dx)
  (* dx (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)))

;(define (integral f a b dx)
;  (define (add-dx x) (+ x dx))
;  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(integral cubes 0 1 0.001)
