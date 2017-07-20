#lang racket
; recursion
;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))

; iteration
(define (product term a next b)
  (define (product_iter a result)
    (if (> a b)
        result
        (product_iter (next a) (* result (term a)))))
  (product_iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (product-integer a b)
  (product identity a inc b))
(product-integer 1 5)

(define (term_pi x)
  (* (/ (- x 1.0) x) (/ (+ x 1.0) x)))
(define (next_pi x)
  (+ x 2))
(define (pi_appro n)
  (* 4.0 (product term_pi 3 next_pi n)))
(pi_appro 10000)
