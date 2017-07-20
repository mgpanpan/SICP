#lang racket

;; 用普通函数实现sum-int(recursion)
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (+ a 1) b))))
(sum-int 1 10)

;; 用普通函数实现sum-cubes(recursion)
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
(define (cube x) (* x x x))
(sum-cubes 1 10)

;; 用普通函数实现pi-sum(recursion)
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))
(* 8 (pi-sum 1 1000))

;; 定义high-order procedure: sum(recursion)
;; 将其余的函数都看成其特例
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

;; 使用iteration方法设计的sum函数
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; sum-int-ho
(define (sum-int-ho a b)
  (sum identity a inc b))
(define (identity x)
  x)
(define (inc x)
  (+ x 1))
(sum-int-ho 1 10)
;; sum-cubes-ho
(define (sum-cubes-ho a b)
  (sum cube a inc b))
(sum-cubes-ho 1 10)
;; pi-sum-ho
(define (pi-sum-ho a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
(* 8 (pi-sum-ho 1 1000))

;; 使用上面的sum函数来求积分
(define (integral f a b dx)
  (* (sum f
       (+ a (/ dx 2.0))
       (lambda (x) (+ x dx))
       b)
     dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;;使用上面的sum函数，并用Simpson公式来求积分
(define (integral-Simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (next x) (+ x (* 2 h)))
    (* (/ h 3) 
       (+ (f a)
          (* 4 (sum f (+ a h) next (+ a (* (- n 1) h))))
          (* 2 (sum f (+ a (* 2 h)) next (+ a (* (- n 2) h))))
          (f (+ a (* n h)))))))
(integral-Simpson cube 0 1.0 10000)

;; 使用iteration方法设计的sum函数
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ result (term a)))))
;  (iter a 0))
