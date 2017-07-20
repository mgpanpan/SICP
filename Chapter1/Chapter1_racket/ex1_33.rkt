#lang racket

; the definition of prime?
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))
;(prime? 13)

; the definition of gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 16 28)

; recursion
;(define (filtered_accumulate filter combiner null-value term a next b)
;  (if (> a b)
;      null-value
;     (combiner (if (filter a)
;                    (term a)
;                    null-value)
;                (filtered_accumulate filter combiner null-value term (next a) next b))))

; iteration
(define (filtered_accumulate filter combiner null-value term a next b)
  (define (filtered_accumulate_iter a result)
    (if (> a b)
        result
        (filtered_accumulate_iter (next a) (combiner result 
                                                     (if (filter a)
                                                         (term a)
                                                         null-value)))))
  (filtered_accumulate_iter a null-value))

(define (sum_square_prime a b)
  (filtered_accumulate prime? + 0 square a next1 b))
(define (next1 x) (+ x 1))
(sum_square_prime 2 13)

(define (identity x) x)

(define (product_relative_prime n)
  (define (is_relative_prime? x)
    (= (gcd n x) 1))
  (filtered_accumulate is_relative_prime? * 1 identity 1 next1 n))
(product_relative_prime 13)