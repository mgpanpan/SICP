#lang racket

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))

;; generate lowest terms for the rational numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;(define (make-rat n d)
;; (let ((g (gcd n d)))
;;    (cons (/ n g) (/ d g))))

;; (define (make-rat n d) (cons n d))


;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (if (and (> n 0) (< d 0))
;        (cons (/ (- n) g) (/ (- d) g))
;        (cons (/ n g) (/ d g)))))

;; exercise 2.1
;; 只有一种例外情况需要考虑，就是分子大于0分母小于0
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and (> (/ n g) 0) (< (/ d g) 0))
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat -1 2))
(print-rat one-half)

(define one-third (make-rat -1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))
