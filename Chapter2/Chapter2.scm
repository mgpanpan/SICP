;; -------------------------------------------------------------------
;; 2.1

;; wishful thinking, use the make-rat, numer and denom
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (numer x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;; ---------------------- 
;; (define make-rat cons)
;; (define numer car)
;; (define denom cdr)
;; ----------------------

;; display, newline: Scheme primitive, neither returns a useful value
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; test
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third));; does not reduced to lowest term

;; use gcd when constructing the pair
;; Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 40 206)
(gcd 206 40)
(gcd 100 10)
(gcd 1 100)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))

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

;; perform the reduction whenever we access the parts of a rational number,
;; rather than we construct it
(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third));; does not reduced to lowest term

;; Exercise 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  )
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (average-point (start-segment segment)
                 (end-segment segment)))
(define (average-point point1 point2)
  (make-point
   (/ (+ (x-point point1) (x-point point2))
      2.0)
   (/ (+ (y-point point1) (y-point point2))
      2.0)))

(define (midpoint-segment segment)
  (define (average-point point1 point2)
    (make-point
     (/ (+ (x-point point1) (x-point point2))
        2.0)
     (/ (+ (y-point point1) (y-point point2))
        2.0)))
  (average-point (start-segment segment)
                 (end-segment segment)))

(define seg (make-segment (make-point 1 1)
                          (make-point 2 3)))
(print-point (midpoint-segment seg))

;; Exercise 2.3
;; -------------------------------------------------------------------
;; support function about segment: length-seg
(define (length-seg segment)
  (define (diff-point point1 point2)
    (make-point (- (x-point point1)
                   (x-point point2))
                (- (y-point point1)
                   (y-point point2))))  
  (define (square-sum-root x y)
    (define (sqrt x)
      (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
      (define (improve guess)
        (define (average x y)
          (/ (+ x y) 2))
        (average guess (/ x guess)))
      (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
      (sqrt-iter 1.0))
    (sqrt (+ (square x)
             (square y))))
  (let ((diff-startend-seg (diff-point (start-segment segment)
                                       (end-segment segment))))
    (square-sum-root (x-point diff-startend-seg)
                     (y-point diff-startend-seg))))
;; test length-seg
(define (average-point point1 point2)
  (make-point
   (/ (+ (x-point point1) (x-point point2))
      2.0)
   (/ (+ (y-point point1) (y-point point2))
      2.0)))
(define seg (make-segment (make-point 1 1)
                          (make-point 2 3)))
(length-seg seg)
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; Method1
;; segment1 and segment2 are the diagonal line of a rectangle
(define (make-rectangle segment1 segment2)
  (cons segment1 segment2))
(define (diagonal1-rect rect) (car rect))
(define (diagonal2-rect rect) (cdr rect))
(define (long-rect rect)
  (let ((seg1 (make-segment (start-segment (diagonal1-rect rect))
                            (start-segment (diagonal2-rect rect))))
        (seg2 (make-segment (start-segment (diagonal1-rect rect))
                            (end-segment (diagonal2-rect rect)))))
    (if (> (length-seg seg1)
           (length-seg seg2))
        seg1
        seg2)))
(define (width-rect rect)
  (let ((seg1 (make-segment (start-segment (diagonal1-rect rect))
                            (start-segment (diagonal2-rect rect))))
        (seg2 (make-segment (start-segment (diagonal1-rect rect))
                            (end-segment (diagonal2-rect rect)))))
    (if (> (length-seg seg1)
           (length-seg seg2))
        seg2
        seg1)))

;; compute the perimeter and area of a rectangle
(define (perimeter-rect rect)
  (* 2 (+ (length-seg (long-rect rect))
          (length-seg (width-rect rect)))))
(define (area-rect rect)
  (* (length-seg (long-rect rect))
     (length-seg (width-rect rect))))

;; test rectangle
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  )

(define exp-rect (make-rectangle (make-segment (make-point 0 3)
                                               (make-point 3 2))
                                 (make-segment (make-point 2 1)
                                               (make-point 1 4))))
(define point1 (start-segment (diagonal1-rect exp-rect)))
(define point2 (start-segment (diagonal2-rect exp-rect)))
(define point3 (end-segment (diagonal1-rect exp-rect)))
(define point4 (end-segment (diagonal2-rect exp-rect)))
(print-point point1)
(print-point point2)
(print-point point3)
(print-point point4)

(define point5 (start-segment (long-rect exp-rect)))
(define point6 (end-segment (long-rect exp-rect)))
(define point7 (start-segment (width-rect exp-rect)))
(define point8 (end-segment (width-rect exp-rect)))
(print-point point5)
(print-point point6)
(print-point point7)
(print-point point8)

(perimeter-rect exp-rect)
(area-rect exp-rect)
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; Method2
;; segment1 and segment2 are the long and width segment of a rectangle
(define (make-rectangle segment1 segment2)
  (cons segment1 segment2))
(define (long-rect rect)
  (car rect))
(define (width-rect rect)
  (cdr rect))
;; compute the perimeter and area of a rectangle (!the same as Method1)
(define (perimeter-rect rect)
  (* 2 (+ (length-seg (long-rect rect))
          (length-seg (width-rect rect)))))
(define (area-rect rect)
  (* (length-seg (long-rect rect))
     (length-seg (width-rect rect))))

;; test
(define exp-rect (make-rectangle (make-segment (make-point 0 3)
                                               (make-point 2 1))
                                 (make-segment (make-point 2 1)
                                               (make-point 3 2))))
(perimeter-rect exp-rect)
(area-rect exp-rect)
;; -------------------------------------------------------------------

;; implementation of pair (cons, car, cdr), book example
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 - CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))
;; (define (test z) (z 3))
;; (test (cons 1 2))
(car (cons 1 2))
(cdr (cons 2 3))

;; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 2 3))

;; Exercise 2.5
;; Chapter1, fast-expt function
(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (factorize m base)
  (define (factorize-iter curr acc)
    (if (= 0 (remainder curr base))
        (factorize-iter (/ curr base) (+ acc 1))
        acc))
  (factorize-iter m 0))
(define (car z) (factorize z 2))
(define (cdr z) (factorize z 3))
;; Note that the parameter of the fast-expt and remainder function
;; have to be nonnegative integers, so the two elements of pairs
;; have to be nonnegative integer
(car (cons 1 2))
(cdr (cons 2 3))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
((zero 1000000) 100)
;; equivalence definition
;; (define (zero f) (lambda (x) x))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(add-1 zero)
(add-1 one)

;; 2.1.4 Interval Arithmetic
;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
;; or implementing sub using add
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
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

(define (print-interval x)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")"))

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 4.46 4.94))
(print-interval r1)
(print-interval r2)
(print-interval (add-interval r1 r2))
(print-interval (sub-interval r1 r2))
(print-interval (mul-interval r1 r2))
(print-interval (div-interval r1 r2))

;; -------------------------------------
;; sicp video 2b, let example
;; let : local context
(let ((z 10))
  (+ z z))
z
;; -------------------------------------

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

;; Exercise 2.11
(define (mul-interval x y)
  (cond ((and (<= (lower-bound x) 0) ;; [-,-]*[-,-],eg. [-2,-1]*[-3,-2]=[2,6]
              (<= (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<= (lower-bound x) 0) ;; [-,-]*[-,+],eg. [-2,-1]*[-1,1]=[-2,2]
              (<= (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<= (lower-bound x) 0) ;; [-,-]*[+,+],eg. [-2,-1]*[1,2]=[-4,-1]
              (<= (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
        ((and (<= (lower-bound x) 0) ;; [-,+]*[-,-],eg. [-2,1]*[-2,-1]=[-2,4]
              (> (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<= (lower-bound x) 0) ;; only this condition needs more than 2 *
              (> (upper-bound x) 0) ;; [-,+]*[-,+],eg. [-2,1]*[-2,1]=[-2,4],[-1,2]*[-1,2]=[-2,4]
              (<= (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (min (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y))
                             (* (upper-bound x) (upper-bound y)))))
        ((and (<= (lower-bound x) 0) ;; [-,+]*[+,+],eg.[-2,1]*[1,2]=[-4,2]
              (> (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (> (lower-bound x) 0) ;; [+,+]*[-,-],eg.[1,2]*[-2,-1]=[-4,-1]
              (> (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (> (lower-bound x) 0) ;; [+,+]*[-,+],eg.[1,2]*[-1,1]=[-2,2]
              (> (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (> (lower-bound x) 0) ;; [+,+]*[+,+],eg.[1,2]*[1,2]=[1,4]
              (> (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))))
;; test
(mul-interval (make-interval -2 -1)
              (make-interval -3 -2))
(mul-interval (make-interval -2 -1)
              (make-interval -1 1))
(mul-interval (make-interval -2 -1)
              (make-interval 1 2))
(mul-interval (make-interval -2 1)
              (make-interval -2 -1))
(mul-interval (make-interval -2 1)
              (make-interval -2 1))
(mul-interval (make-interval -1 2)
              (make-interval -1 2))
(mul-interval (make-interval -2 1)
              (make-interval 1 2))
(mul-interval (make-interval 1 2)
              (make-interval -2 -1))
(mul-interval (make-interval 1 2)
              (make-interval -1 1))
(mul-interval (make-interval 1 2)
              (make-interval 1 2))

;; using "center width" instead of "lower upper"
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define r1 (make-center-width 6.8 0.68))
(center r1)
(width r1)

;; Exercise 2.12
(define (make-center-percent c width-percent)
  (let ((width (/ (* c width-percent) 100.0)))
    (make-interval (- c width)
                   (+ c width))))
(define (percent x)
  (* 100 (/ (- (upper-bound x) (lower-bound x))
            (+ (lower-bound x) (upper-bound x)))))
(define r1-test (make-center-percent 6.8 10))
(center r1-test)
(percent r1-test)

;; Exercise 2.13
;; premise: 1. small percentage tolerances, 2. all numbers are positive
;; simple formula : percent = percent1 + percent2
(define r1 (make-center-percent 6.8 10))
(define r2 (make-center-percent 4.7 5))
(define r (mul-interval r1 r2))
(percent r1)
(percent r2)
(percent r)

;; two different ways of compute the value of a parallel equivalent resistance
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define par1-r1r2
  (par1 r1 r2))
(define par2-r1r2
  (par2 r1 r2))
;; par1-r1r2 != par2-r1r2

;; -------------------------------------------------------------------
;; 2.2
(cons 1
      (cons 2
            (cons 3
                  (cons 4
                        (cons 5 '())))))
(list 1 2 3 4 5)
(define one-through-five (list 1 2 3 4 5))
(car one-through-five)
(cdr one-through-five)
(car (cdr one-through-five))
(cons 10 one-through-five)
(cons one-through-five 5)

;; list-ref: takes as arguments a list and a number n, and returns
;; the nth item of the list
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)
(list-ref squares 4)
(list-ref squares 5)

;; length: returns the number of items in list
;; recursive version
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

;; iterative version
(define (length items)
  (define (length-iter items counter)
    (if (null? items)
        counter
        (length-iter (cdr items) (+ counter 1))))
  (length-iter items 0))
(length odds)

(append squares odds)
(append odds squares)

;; a recursive implementation of append
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17
;; premise: parameter is a nonempty list
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
;; wrong, should return a list contains only the last element
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))
(last-pair odds)
(last-pair squares)

;; Exercise 2.18
;; reverse : takes a list as argument and returns a list of the same
;;           elements in reverse order.
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items '()))
(reverse odds)
(reverse squares)

;; Exercise 2.20
;; recursive version
(define (same-parity a . n)
  (define (find type? n)
    (cond ((null? n) '())
          ((type? (car n)) (cons (car n) (find type? (cdr n))))
          (else (find type? (cdr n)))))
  (if (odd? a)
      (cons a (find odd? n))
      (cons a (find even? n))))

;; (define (same-parity a . n)
;;   (define (find type? n)
;;     (define (find-iter type? n result)
;;       (cond ((null? n) result)
;;             ((type? (car n)) (find-iter type? (cdr n)
;;                                        (cons (car n) result)))
;; ;;                                       (append result (cons (car n) '())))))
;;             (else (find-iter type? (cdr n) result))))
;;     (find-iter type? n '()))
;;   (if (odd? a)
;;       (cons a (find odd? n))
;;       (cons a (find even? n))))

;; iterative version
(define (same-parity a . n)
  (define (find type? n)
    (define (find-iter type? n result)
      (cond ((null? n) result)
            ((type? (car n)) (find-iter type? (cdr n)
                                        (append result (cons (car n) '()))))
            (else (find-iter type? (cdr n) result))))
    (find-iter type? n '()))
  (if (odd? a)
      (cons a (find odd? n))
      (cons a (find even? n))))

(same-parity 1 2)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)

;; Mapping over lists

;; scale-list : scales each number in a list by a given factor
;; recursive version
(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
;; iterative version, too complex??
(define (scale-list items factor)
  (define (scale-list-iter items factor result)
    (if (null? items)
        result
        (scale-list-iter (cdr items) factor
                         (append result (cons (* (car items) factor) '())))))
  (scale-list-iter items factor '()))
(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x))
     (list 1 2 3 4 5 6))
(map square (list 1 2 3 4 5 6))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5 6) 100)

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(square-list (list 1 2 3 4 5 6))
(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4 5 6))

;; -------------------------------------------------------------------
;; Exercise 2.22
;; attept to use an iterative process to compute the square of a list,
;; but the result is a reversed version.
(define (square-list items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (square (car items))
                                result))))
  (iter items '()))
(square-list (list 1 2 3 4 5 6))
;; we can compare the above procedure to the "reverse" procedure:
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items)
                                        result))))
  (reverse-iter items '()))
(reverse (list 1 2 3 4 5 6))

;; attept to modify the above solution, but the result is not a list
(define (square-list items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons result
                                (square (car items))))))
  (iter items '()))
(square-list (list 1 2 3 4 5 6))
;; result: ((((((() . 1) . 4) . 9) . 16) . 25) . 36)

;; modify using the append procedure
(define (square-list items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (append result (cons (square (car items)) '())))))
  (iter items '()))

;; or
(define (square-list items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (append result (list (square (car items)))))))
  (iter items '()))
(square-list (list 1 2 3 4 5 6))
;; -------------------------------------------------------------------

;; Exercise 2.23
;; can't use if, cause there should only be one command under each branch
;; of "if" special-case
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88 11 2 3 4 5 6))

;; 2.2.2 Hierarchical Structures

;; ((1 2) 3 4)
(cons (list 1 2) (list 3 4))
(define (length items)
  (define (length-iter items result)
    (if (null? items)
        result
        (length-iter (cdr items) (+ result 1))))
  (length-iter items 0))
(length (list 1 2 3 4 5 6))
(define tree-test (cons (list 1 2) (list 3 4)))
(length tree-test)
(define tree-test2 (cons (list 1 2) (list 3 (list 4 5 6))))
(length tree-test2)
(define tree-test3 (cons (list 1 2) (cons 3 (list 4 5 6))))
(length tree-test3)
(define tree-test4 (cons (list 1 2) (cons (list 4 5 6) (list 3))))
(length tree-test4)
(define tree-test5 (cons (list 1 2) (cons (list 4 5 6) 3)))
(length tree-test5)
;;! ;The object 3, passed as the first argument to cdr, is not the correct type.
;; tree-test5 is not a list!!!

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)
(define y (list x x))
(length y)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves x)
(count-leaves y)

;; Exercise 2.24
(define x (list 1 (list 2 (list 3 4))))

;; Exercise 2.25
(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))
(define y (list (list 7)))
(car (car y))
(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;; Exercise 2.27 ???
(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 4) (list 5 6)))
;;
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items '()))

;;
(define (reverse items)
  (if (null? items)
      '()
      (cons (reverse (cdr items)) (car items))))

(reverse x)
(reverse y)

(define (deep-reverse items)
  (define (deep-reverse-iter items result)
    (if (null? items)
        result
        (deep-reverse-iter (cdr items) (cons (deep-reverse-iter
                                              (car items) '())
                                             result))))
  (deep-reverse-iter items '()))
(deep-reverse x)

;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(define tree-test (list 1 (list 2 (list 3 4) 5) (list 6 7)))
tree-test
(scale-tree tree-test 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree tree-test 100)

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(square-tree tree-test)
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree tree-test)

;; Exercise 2.31 tree map
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor))
            tree))
(define (square-tree tree)
  (tree-map (lambda (x) (square x))
            tree))
(define tree-test (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(scale-tree tree-test 10)
(square-tree tree-test)

;; 2.2.3 Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(sum-odd-squares tree-test)

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
(even-fibs 30)

;; signal-flow plan : map, filter, accumulate, enumerate
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5 6 7 8 9 10 21 22 23 24 25 26))
(filter even? (list 1 2 3 4 5 6 7 8 9 10 21 22 23 24 25 26))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5 10 9 8 7 6))

;; the enumerate method for trees and for integers is different
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
(enumerate-interval 2 1000)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define tree-test (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(enumerate-tree tree-test)

;; sum-odd-squares : 1. enumerate the sequence of leaves of the tree
;;                   2. filter
;;                   3. map square
;;                   4. accumulate
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(sum-odd-squares tree-test)

;; even-fibs : 1. enumerate the sequence of a interval
;;             2. map fib
;;             3. filter
;;             4. accumulate
(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
(even-fibs 10)
(even-fibs 20)

;; list-fib-squares
;; 1. enumerate interval 0~n
;; 2. map fib, map squares
;; 3. no filter
;; 4. accumulate
(define (list-fib-squares n)
  (accumulate cons
              '()
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)

;; product-of-squares-of-odd-elements
;; 1. input parameter is already a sequence, don't need to enumerate
;; 2. filter odd
;; 3. map squares
;; 4. accumulate *
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5 6 7))

;; Exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))
(map square (list 1 2 3 4 5 6))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))
(append (list 1 2 3 4 5) (list 10 9 8 7 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))
(length (list 1 2 3 4 5 6 10 9 8 7 6))

;; section 2.3
(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(a b c)
'(a b c)
(cdr '(a b c))
'()

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(pear apple banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
(memq '(apple sauce) '(x (apple sauce) y apple pear)) ;; ?????
(eq? '(apple sauce) '(apple sauce)) ;; ?????
(eq? 'apple 'apple)

;; Exercise 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;; Exercise 2.54
(define (my-equal? a b)
  (cond ((not (= (length a) (length b))) #f)
        ((null? a) #t)
        ((and (pair? (car a)) (pair? (car b))) (equal? (car a) (car b)))
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))
(my-equal? '(a (b c (d eef)) (abcd)) '(a (b c (d eef)) (abcd)))
(my-equal? '(a (b c (d eef)) (abcd)) '(a (b c (d eef)) (abce)))
(my-equal? '(a) '(b))
(my-equal? '(a) '(a))
(my-equal? '(a b c d) '(a b c))
(length '(a b c d))

;; Exercise 2.55
(car ''abrac)
'

;; 2.3.2 Example: Symbolic Differentiation
;; wishful thinking, assume we already have procedures to implement
;; the following selectors, constructors and predicates:

;; (variable? e)          ;; Is e a variable?
;; (same-variable? v1 v2) ;; Are v1 and v2 the same variable?
;; (sum? e)               ;; Is e a sum?
;; (addend e)             ;; Addend of the sum e
;; (augend e)             ;; Augend of the sum e
;; (make-sum a1 a2)       ;; Construct the sum of a1 and a2
;; (product? e)           ;; Is e a product?
;; (multiplier e)         ;; Multiplier of the product e
;; (multiplicand e)       ;; Multiplicand of the product e
;; (make-product m1 m2)   ;; Construct the product of m1 and m2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Representing algebraic expressions
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend e) (cadr e))
(define (augend e) (caddr e))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y) 'y)
(deriv '(* (* x y) (+ x 3)) 'x)

;; simplified
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; (define (make-sum a1 a2)
;;   (cond ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else (list '+ a1 a2))))
;; (define (make-product m1 m2)
;;   (cond ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list '* m1 m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y) 'y)
(deriv '(* (* x y) (+ x 3)) 'x)

;; Exercise 2.56
(define (** base exponent)
  (define (iter exponent result)
    (if (= exponent 0)
        result
        (iter (- exponent 1) (* result base))))
  (iter exponent 1))
(** 1 2)
(** 1 0)
(** 100 0)
(** 0 100)
(** 2 3)
(** 2 10)
(** 3 3)

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define test-express '(** a 10))
(base test-express)
(exponent test-express)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(deriv '(** x 10) 'x)

;; debug process
(define test-express '(** x 10))
(exponent test-express)
(base test-express)
(make-exponentiation (base test-express) (make-sum (exponent test-express) -1))
(make-exponentiation 'x 9)

(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)
(deriv '(** x 3) 'x)
(deriv '(* (+ x (** y 10)) y) 'y)

;; Exercise 2.57
;; Extend the differentiation program to handle sums and products of
;; arbitrary numbers of (two or more) terms
;; change the augend and multiplicand function

(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend e) (cadr e))

;; (define (augend e) (cons (car e) (cddr e)))
(define (augend e)
  (if (= (length e) 3)
      (caddr e)
      (cons (car e) (cddr e))))

(augend '(+ a b c d e f))
(augend '(+ a b))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p) (cadr p))
;; (define (multiplicand p) (cons (car p) (cddr p)))
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons (car p) (cddr p))))

(multiplicand '(* a b c d e f g))

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;; numercial exp function **
(define (** base exponent)
  (define (iter exponent result)
    (if (= exponent 0)
        result
        (iter (- exponent 1) (* result base))))
  (iter exponent 1))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'y)

(multiplicand '(* x y (+ x 3)))

;; Exercise 2.58, a
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))
(sum? '(a + b))
(sum? '(a * b))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(make-sum 'a 'b)
(make-sum 'a 0)
(make-sum 'a '0)
(make-sum 0 'b)

(define (addend e) (car e))
(define (augend e) (caddr e))
(addend '(a + b))
(augend '(a + b))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(product? '(a * b))
(product? '(a + b))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(make-product 'a 'b)
(make-product 1 2)

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(multiplier '(a * b))
(multiplicand '(a * b))

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list  base '** exponent))))
(define (exponentiation? e)
  (and (pair? e) (eq? (cadr e) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))

;; numercial exp function **
(define (** base exponent)
  (define (iter exponent result)
    (if (= exponent 0)
        result
        (iter (- exponent 1) (* result base))))
  (iter exponent 1))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(3 * (x + (y + 2))) 'x)
(deriv '(3 * (x + (y + 2))) 'y)
(deriv '(3 * (x * (y + x))) 'x)
(deriv '(3 * (x * (y + x))) 'y)
(deriv '(3 * (x + (y ** x))) 'y)
(deriv '(3 * (y + (x ** 1))) 'x)
(deriv '(3 * (y + (x ** 2))) 'x)
(deriv '(3 * (y + (x ** 3))) 'x)

;; 2.3.3 Example: representing sets
;; 1. sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
  (define (iter set1 result)
    (cond ((null? set1) result)
          ((element-of-set? (car set1) set2)
           (iter (cdr set1) result))
          (else (iter (cdr set1) (cons (car set1) result)))))
  (iter set1 set2))
(define set1 '(a b c d e))
(define set2 '(f g h i j e d c))
(element-of-set? 'c set1)
(element-of-set? 'f set1)
(adjoin-set 'c set1)
(adjoin-set 'x set1)
(intersection-set set1 set2)
(union-set set1 set2)

;; Exercise 2.60
;; represent a set as a list allow duplicates
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set2)
  (append set1 set2))
;; (append '(a b c d (e f)) '(e f g h i j k))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define set1 '(a b c d e))
(define set2 '(f g h i j e d c))
(element-of-set? 'c set1)
(element-of-set? 'f set1)
(adjoin-set 'c set1)
(adjoin-set 'x set1)
(intersection-set set1 set2)
(union-set set1 set2)

;; 2. sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(element-of-set? '3 '(1 2 3 4 5))
(element-of-set? '3 '(1 4 5 6 3))
(element-of-set? '3 '(1 2 4 5 6))
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))
        ))
(adjoin-set '3 '(1 2 3 4 5))
(adjoin-set '3 '(1 2 4 5))
(adjoin-set '3 '(2 100))
(adjoin-set '3 '(1))
(adjoin-set '100 '(1 2 3 4 5 6))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1
                               (intersection-set (cdr set1)
                                                 (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1)
                                           set2))
              ((< x2 x1) (intersection-set set1
                                           (cdr set2)))))))
(intersection-set '(1 2 3 4 5) '(2 3 4 5 6))
(intersection-set '(1 2 3 4 5) '(10 11 12 13 14 15))
(intersection-set '(1 2 3 4 5) '(1 2 3 4 5))
(intersection-set '(1 2 3 4 5) '(3 4 5 6))
(intersection-set '(10 11 12 13 14 15) '(9 10 11 12))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2)
               (union-set set1 (cdr set2))))))
(union-set '(1 2 3 4 5) '(2 3 4 5 6))
(union-set '(1) '(2))
(union-set '(1) '())
(union-set '() '(1))
(union-set '(1 2 3 4 5) '(10 11 12 13 14 15))
(union-set '(10 11 12) '(1 2 3 6))
(union-set '(10 11 13) '(1 2 10 13))

;; 3. sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define set1 (make-tree 7 (make-tree 3
                                     (make-tree 1 '() '())
                                     (make-tree 5 '() '()))
                        (make-tree 9 '() (make-tree 11 '() '()))))
(entry set1)
(left-branch set1)
(right-branch set1)
(entry (left-branch set1))
(left-branch (left-branch set1))
(right-branch (left-branch set1))
(left-branch (right-branch set1))
(right-branch (right-branch set1))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(element-of-set? 7 set1)
(element-of-set? 100 set1)
(element-of-set? 5 set1)
(element-of-set? 9 set1)
(element-of-set? -1 set1)
(element-of-set? 3 set1)
(element-of-set? 11 set1)

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
(adjoin-set 10 set1)
(adjoin-set 5 set1)
(adjoin-set 4 set1)
(adjoin-set 2 set1)
(adjoin-set 8 set1)
(adjoin-set -1 set1)

(define set-fig2.17 '())
(adjoin-set 5 (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 set-fig2.17)))))

;; Exercise 2.63
(define set1 (make-tree 7 (make-tree 3
                                     (make-tree 1 '() '())
                                     (make-tree 5 '() '()))
                        (make-tree 9 '() (make-tree 11 '() '()))))
(define set2 (make-tree 3 (make-tree 1 '() '())
                        (make-tree 7 (make-tree 5 '() '())
                                   (make-tree 9 '() (make-tree 11 '() '())))))
(define set3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '())
                        (make-tree 9 (make-tree 7 '() '())
                                   (make-tree 11 '() '()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(tree->list-1 set1)
(tree->list-1 set2)
(tree->list-1 set3)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
(tree->list-2 set1)
(tree->list-2 set2)
(tree->list-2 set3)

;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
(list->tree '(1 3 5 7 9 11))
(list->tree '(5 9 7 11 3 1))

;; Exercise 2.65

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
(define elmt1 '(1 "Peter"))
(define elmt2 '(3 "Mary"))
(define elmt3 '(5 "Jack"))
(define elmt4 '(7 "John"))
(define elmt5 '(9 "Tom"))
(define (key elmt)
  (car elmt))
(define set-of-records (make-tree elmt4
                                  (make-tree elmt2
                                             (make-tree elmt1
                                                        '() '())
                                             (make-tree elmt3
                                                        '() '()))
                                  (make-tree elmt5 '() '())))
set-of-records
(lookup 1 set-of-records)
(lookup 2 set-of-records)
(lookup 3 set-of-records)
(lookup 4 set-of-records)
(lookup 5 set-of-records)
(lookup 6 set-of-records)
(lookup 7 set-of-records)
(lookup 8 set-of-records)

;; section 2.4

;; 2.4.1
;; wishful thinking
;; assume that procedures: make-from-real-imag, make-from-mag-ang,
;; real-part, imag-part, magnitude and angle are already constucted.
;; these are the constructors and selectors of the abstract data--complex number
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; the implementation of the constructors: make-from-real-imag, make-from-mag-ang
;; and the selectors: real-part, imag-part, magnitude and angle depends on the
;; representation methods of the complex number, different representations
;; lead to different implementations of the constructors and selectors.

;; complex number in rectangular form:
(define (make-from-real-imag x y)
  (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a))
        (* r (sin a))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

;; test
(define z1 (make-from-real-imag 1 1))
(define z2 (make-from-real-imag 1 -1))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

;; complex number in polar form
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x)
                 (square y)))
        (atan y x)))
(define (make-from-mag-ang r a)
  (cons r a))
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))

;; test
(define z1 (make-from-real-imag 1 1))
(define z2 (make-from-real-imag 1 -1))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

;; the same implementation of add-complex, sub-complex, mul-complex and
;; div-complex will work with either of the two representation forms.

;; 2.4.2 tagged data
;; include a type tag as a part of each complex number
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; rename the procedure of the two representation forms
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a))
                             (* r (sin a)))))
(define (real-part-rectangular z) (car (contents z)))
(define (imag-part-rectangular z) (cdr (contents z)))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x)
                                (square y)))
                       (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car (contents z)))
(define (angle-polar z) (cdr (contents z)))

;; generic selector is implemented as a procedure that checks the tag of its
;; argument and calls the appropriate procedure for handling data of that
;; type.
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular z))
        ((polar? z)
         (real-part-polar z))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular z))
        ((polar? z)
         (imag-part-polar z))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular z))
        ((polar? z)
         (magnitude-polar z))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular z))
        ((polar? z)
         (angle-polar z))
        (else (error "Unknown type -- ANGLE" z))))

;; construct recatangular numbers whenever we have real and imaginary parts
;; and construct polar numbers whenever we have magnitudes and angles
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; test
(define z1 (make-from-real-imag 1 1))
(define z2 (make-from-real-imag 1 -1))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

(define pi 3.1415)
(define z1 (make-from-mag-ang 1 0))
(define z2 (make-from-mag-ang 1 (/ pi 2)))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

;; 2.4.3 Data-Directed Programming

;;;-----------
;;;from section 3.3.3 for section 2.4.3
;;; to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;;; ------------------------------------------------------------------

;; (put <op> <type> <item>)
;; install the <item> in table, indexed by the <op> and the <type>
;; (get <op> <type>)
;; looks up the <op>, <type> entry in the table and returns the item
;; found there. If no item is found, get returns false.

(define (install-rectangular-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x)
                   (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;; these selectors do not change at all if a new representation is
;; added to the system.

;; as in 2.4.2, we construct rectangular numbers whenever we have real
;; and imaginary parts, and polar numbers whenever we have magnitudes
;; and angles. !This is not unique.
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; test
(install-rectangular-package)
(install-polar-package)

(define z1 (make-from-real-imag 1 1))
(define z2 (make-from-real-imag 1 -1))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

(define pi 3.1415)
(define z1 (make-from-mag-ang 1 0))
(define z2 (make-from-mag-ang 1 (/ pi 2)))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

;; Exercise 2.73
;; -------------------------------------------------------------------
;; section 2.3.2 version of symbolic differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (** base exponent)
  (define (iter exponent result)
    (if (= exponent 0)
        result
        (iter (- exponent 1) (* result base))))
  (iter exponent 1))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend e) (cadr e))
(define (augend e) (caddr e))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(deriv '(* (+ x (** y 10)) y) 'y)
;; -------------------------------------------------------------------

(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))

  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (multiplicand operands)
                   (deriv (multiplier operands) var))))
  
  ;; interface to the rest of the system
  (put 'deriv '* deriv-product)
  'done)

(define (install-deriv-exponent-package)
  ;; internal procedures
  (define (** base exponent)
    (define (iter exponent result)
      (if (= exponent 0)
          result
          (iter (- exponent 1) (* result base))))
    (iter exponent 1))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation base exponent)
    (cond ((=number? base 0) 0)
          ((=number? base 1) 1)
          ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent)) (** base exponent))
          (else (list '** base exponent))))
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))

  (define (deriv-exponent operands var)
    (make-product (exponent operands)
                  (make-exponentiation (base operands)
                                       (make-sum (exponent operands) -1))))
  
  ;; interface to the rest of the system
  (put 'deriv '** deriv-exponent)
  'done)

(install-deriv-sum-package)
(install-deriv-product-package)
(install-deriv-exponent-package)

(deriv '(+ (* x y) y) 'y)
(deriv '(* (+ x (** y 10)) y) 'y)
(deriv '(** x 10) 'x)
(define quartic '(+ (* a (* x x))
                    (+ (* b x)
                       c)))
(deriv quartic 'x)
(deriv quartic 'a)
(deriv quartic 'b)
(deriv quartic 'c)

;; Message passing
;; .....

;; section 2.5 systems with generic operations

;;;----------- -------------------------------------------------------
;;;from section 3.3.3
;;; to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;; -------------------------------------------------------------------

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; --------------------
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; --------------------------
(define (install-scheme-number-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag 'scheme-number x))
  ;; interface to rest of the system
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; -------------------------
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  ;; interface to rest of the system
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; ------------------------
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  ;; interface to the rest of the system
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x)
                   (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(install-scheme-number-package)
(define n1 (make-scheme-number 100))
(define n2 (make-scheme-number 1000))

(install-rational-package)
(define r1 (make-rational 2 3))
(define r2 (make-rational 3 4))

(install-complex-package)
;; must first install-rectangular-package
;; (define c1 (make-complex-from-real-imag 1 2))
;; must first install-polar-package
;; (define c2 (make-complex-from-mag-ang 1 (/ 3.14159 2)))
  
(install-rectangular-package)
(install-polar-package)
(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-mag-ang 1 (/ 3.14159 2)))

(add n1 n2)
(sub n1 n2)
(mul n1 n2)
(div n1 n2)

(add r1 r2)
(sub r1 r2)
(mul r1 r2)
(div r1 r2)

(add c1 c2)
(sub c1 c2)
(mul c1 c2)
(div c1 c2)

;; Exercise 2.77
(magnitude c1)

;; edit the complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  ;; interface to the rest of the system
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)
(magnitude c1)

;; Exercise 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else "Bad tagged datum -- CONTENTS" datum)))
(define (attach-tag type-tag contents)
  (if (number? contents)
       contents
       (cons type-tag contents)))
(define n1 (make-scheme-number 100))
(define n2 (make-scheme-number 1000))
(add n1 n2)
(sub n1 n2)
(mul n1 n2)
(div n1 n2)

;; Exercise 2.79, 2.80
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag 'scheme-number x))
  ;; interface to rest of the system
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  ;; interface to rest of the system
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (/ (numer x) (denom x))
                        (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  ;; interface to the rest of the system
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'equ? '(complex complex)       
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-scheme-number-package)
(equ? n1 n2)
(equ? n1 n1)
(equ? n2 n2)
(equ? (make-scheme-number 1)
      (make-scheme-number 2))
(=zero? n1)
(=zero? (make-scheme-number 0))

(install-rational-package)
(equ? r1 r2)
(equ? r1 r1)
(equ? r2 r2)
(=zero? (make-rational 1 1))

(install-complex-package)
(equ? c1 c1)
(equ? c1 c2)
(equ? c2 c2)
(=zero? (make-complex-from-real-imag 1 0))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-mag-ang 0 10))
(=zero? (make-complex-from-mag-ang 1 10))

;; coercion convert

;; Exercise 2.83
