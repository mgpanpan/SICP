#lang racket

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p-start p-end)
  (cons p-start p-end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
