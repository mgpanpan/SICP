;; wishful thinking, use the make-rat, numer and denom
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

(define make-rat cons)
(define numer car)
(define denom cdr)
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third));;does not reduced to lowest term

;; use gcd when constructing the pair
;; Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 40 206)
(gcd 206 40)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and (> (/ n g) 0) (< (/ d g) 0))
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))
(print-rat (make-rat 206 40))
(print-rat (make-rat (- 206) 40))
(print-rat (make-rat 206 (- 40)))
(print-rat (make-rat (- 206) (- 40)))

;; Exercise 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2.0)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2.0)))

(print-point (midpoint-segment (make-segment (make-point 1 1)
                                             (make-point 2 3))))
;; implemention of pair (cons, car, cdr)
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

;; Exercise 2.5
(define (cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))
;; 第一章中所用的求幂函数fast-expt
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
;; Iterative
(define (factorize m base)
  (define (factorize-iter curr acc)
    (if (= 0 (remainder curr base))
        (factorize-iter (/ curr base) (+ acc 1))
        acc))
  (factorize-iter m 0))
(define (car z) (factorize z 2))
(define (cdr z) (factorize z 3))
;; 注意这里用的这种方法由于用了第一章的fast-expt以及remainder函数
;; 要求pairs的两个元素必须是nonnegative integers

;; Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b)
  (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display ")"))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
;;通过加法实现减法
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;; Exercise 2.9
(define (width x) (/ (- (upper-interval x)
                        (lower-interval x))
                     2.0))
(mul-interval (make-interval 1 3) (make-interval 1 3))
(mul-interval (make-interval 0 2) (make-interval 1 3))

(define r1 (make-interval 6.12 7.48))
(print-interval r1)
(define r2 (make-interval 4.46 4.94))
(print-interval r2)
(print-interval (add-interval r1 r2))
(print-interval (sub-interval r1 r2))

;; 注意，下面用(R1*R2)/(R1+R2)计算的结果和书中不同
(print-interval (div-interval (mul-interval r1 r2)
                              (add-interval r1 r2)))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1.0 1.0)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(print-interval (par1 r1 r2))
(print-interval (par2 r1 r2))

;; Exercise 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "Divisor interval contains zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
(div-interval (make-interval 1.0 1.0)
              (make-interval -1.0 1.0))

;; using "center width" instead of "lower upper"
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2.0))
(define test-x (make-center-width 3.5 0.15))
(center test-x)
(width test-x)

;; Exercise 2.12
(define (make-center-percent c width-percent)
  (let ((width (/ (* c width-percent) 100.0)))
    (make-interval (- c width)
                   (+ c width))))
(make-center-percent 6.8 10)
(define test (make-center-percent 6.8 10))
(define (percent x)
  (/ (* (- (upper-bound x) (lower-bound x))
        50)
     (/ (+ (lower-bound x) (upper-bound x))
        2)))

(center test)
(percent test)

(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons one-through-four 5)	;; 需要注意

(define (list-reg items n)
  (if (= n 0)
      (car items)
      (list-reg (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)

;; Recursive
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

;; Iterative
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(append squares odds)
(append odds squares)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17
;; 题目中已经给出条件: items非空
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(last-pair (list 23 72 149 34))

;; Exercise 2.18

;; wrong solution
;; (define (reverse items)
;;   (if (null? (cdr items))
;;       (car items)
;;       (cons (reverse (cdr items)) (car items))))
;; (reverse (list 1 4 9 16 25))

;; 用类似于压栈的思想
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items '()))
(reverse (list 1 4 9 16 25))

;; Exercise 2.19
(define us-coins (list 25 50 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values)) 
(cc 100 us-coins)
(cc 100 uk-coins)

;; Exercise 2.20
(define (same-parity a . n)
  (define (find type n)
    (cond ((null? n) '())
          ((type (car n)) (cons (car n) (find type (cdr n))))
          (else (find type (cdr n)))))
  (if (odd? a)
      (cons a (find odd? n))
      (cons a (find even? n))))
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))
(square-list (list 1 2 3 4 5))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map (proc (cdr items))))))
(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x))
     (list 1 2 3 4 5))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

;; Exercise 2.22
;; squared the list and reverse
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; Exercise 2.23
;; 不能用if，因为if的每个条件下只能有一条命令
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)

(define xx (list x x))
(length xx)
(count-leaves xx)

;; Exercise 2.24
(define x (list 1 (list 2 (list 3 4))))

;; Exercise 2.25
(define list1 (list 1 3 (list 5 7) 9))
(define list2 (list (list 7)))
(define list3 (cons 1
                    (cons 2
                          (cons 3
                                (cons 4
                                      (cons 5
                                            (cons 6 7)))))))
list1
list2
list3

(define (sum-of numlist)
  (if (null? numlist) 0
      (+ (car numlist)
         (sum-of (cdr numlist)))))
(sum-of '(1 2 3 4))
(sum-of '(1 2 3 (4)))
