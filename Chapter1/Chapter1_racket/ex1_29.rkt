#lang racket

; linear recursion
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

; linear iteration
(define (sum term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral_Simpson f a b n)
  (define h (/ (- b a) n))
  (define (next_x x)
    (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (* 2 (sum f (+ a (* h 2)) next_x (- b (* h 2))))
        (* 4 (sum f (+ a h) next_x (- b h)))
        (f b))))
(define (cube x)
  (* x x x))
(integral_Simpson cube 0 1.0 10000)
