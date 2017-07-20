#lang racket

(define (cont-frac n d k)
  (define (cont-frac-recursion i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recursion (+ i 1))))))
  (cont-frac-recursion 1.0))

;(define (cont-frac n d k)
;  (define (cont-frac-iter i result)
;    (if (< i 1)
;        result
;        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
;  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)

(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder i 3) 2)
                 (+ 2 (* 2 (/ (- i 2) 3)))
                 1))
           1000)

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* 2.0 i) 1))
  (cont-frac n d k))

(tan-cf (/ 3.14 4) 10000)