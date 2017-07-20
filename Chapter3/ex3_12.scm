;; the append definition in chapter2
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define x '(a b c d))
(define y '(d e f g))
(cons x y)
(append x y)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; last-pair is a procedure that returns the last pair in its argument
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define w (append! x y))
w
x
(cdr x)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(last-pair z)
;; infinite loop

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
(define w (mystery v))

