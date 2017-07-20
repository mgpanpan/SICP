;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 用define进行最简单的抽象
;; 用一个简单的名字去引用一个组合运算(combination)的结果
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

;; 用define进行过程定义(compound procedure)
(define (square x) (* x x))
(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-square x y)
  (+ (square x) (square y)))
(sum-of-square 3 4)

(define (f a)
  (sum-of-square (+ a 1) (* a 2)))
(f 5)

;; 1.1.6 Conditional Expressions
;; cond, if, and, or, not
;;;;;;;;;;;;;;;;;;;;;;; define abs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(abs -1)
(abs 0)
(abs 1)

(define x 6)
(and (> x 5)
     (< x 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; define >= ;;;;;;;;;;;;;;;;;;;;;;;
(define (>= x y)
  (or (> x y)
      (= x y)))
(define (>= x y)
  (not (< x y)))
(>= 1 1)
(>= 1 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum-square-max : retrun the sum of the squares of the two larger numbers
(define (sum-of-square a b)
  (+ (square a)
     (square b)))
(define (square x)
  (* x x))
(define (sum-square-max a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-square a b)
          (sum-of-square a c))
      (if (> a c)
          (sum-of-square a b)
          (sum-of-square b c))))

;; Exercise 1.3 my answer before
(define (sum-square-max a b c)
  (if (> a b)
      (if (> a c)
          (if (> b c)
              (sum-of-square a b)
              (sum-of-square a c))
          (sum-of-square c a))
      (if (> b c)
          (if (> a c)
              (sum-of-square b a)
              (sum-of-square b c))
          (sum-of-square c b))))

;; Exercise 1.3 sicp.org answer
(define (sum-square-max a b c)
  (define x (if (> a b) a b))
  (define y (if (< a b) a b))
  (define z (if (> y c) y c))
  (+ (* x x) (* z z)))

;; Test
(sum-square-max 3 4 5)
(sum-square-max 3 5 4)
(sum-square-max 4 3 5)
(sum-square-max 4 5 3)
(sum-square-max 5 3 4)
(sum-square-max 5 4 3)
(sum-square-max 3 3 3)

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a + abs(b)
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -)
   a b))
;; Test
(a-plus-abs-b 1 -10)
(a-plus-abs-b 1 0)
(a-plus-abs-b 1 10)
(a-plus-abs-b -1 10)

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We can use this test to determine whether the interpreter is using
;; applicative-order evaluation or normal-order evaluation.
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; 执行时，进入死循环，说明解释器采用的是应用序(applicative-order)
;; 如果解释器采用的是正则序,则运算的结果是０，不会进入无限循环
;; test result : MIT-Scheme is using applicative-order

;;;;;;;;; Square Roots by Newton's Method(Alexandria Method) ;;;;;;;;;
;; 1.1.7
;; 采用的是迭代法(iterative)
;; 可以看到Scheme不需要专门的迭代/循环(iterative/looping)结构来让计算机重复执行
;; 某些指令，仅仅用函数调用就能完成迭代

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
;; 初始值总是取1.0
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test using new-if(define by cond) to Newton's Method
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
;; Result: ;Aborting!: maximum recursion depth exceeded
;; 执行结果显示，程序未能正确执行，原因在于进入了一个无限递归
;; if是一个special form，有其特定的执行规则，首先判断predicate是否成立，如果成立
;; 则执行then-clause，否则执行else-clause，而这里的new-if虽然是由cond定义的
;; cond是一个特殊形式，但是new-if本身是一个普通过程，所以执行时需要按照应用式
;; (applicative order)进行代换，applicative order要求所有的operand都要计算
;; 出来，才能将operator应用到相应的operand上，这就造成了每次执行new-if时，都会执
;; 行sqrt-iter，从而造成了无限递归

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 原本的good-enough?是根据guess^2是否接近要开方的值x
;; 本题要求两次预测的值之差很小
(define (good-enough? guess pre-guess x)
  (< (abs (- guess pre-guess)) 0.01))
(define (sqrt-iter guess pre-guess x)
  (if (good-enough? guess pre-guess x)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))
(sqrt 9)
(sqrt 10000000000000000)

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))
(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x))
     0.001))
(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))
(define (cube x)
  (* x x x))
(define (cube-root x)
  (cube-root-iter 1.0 x))

;; Change the good-enough-cube? procedure
(define (cube-root-iter guess pre-guess x)
  (if (good-enough-cube? guess pre-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))
(define (good-enough-cube? guess pre-guess)
  (< (abs (- guess pre-guess))
     0.001))
(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))
(define (cube x)
  (* x x x))
(define (cube-root x)
  (cube-root-iter 1.0 0.0 x))

(cube-root 27)
(cube-root 1000)
(cube-root 64)
(cube-root 1)
(cube-root -27)
(cube-root -1000)
(cube-root -64)
(cube-root -1)

;;;;;;;;;;;;;;;;;;;; 1.1.8 internal definitions;;;;;;;;;;;;;;;;;;;;;;;
;; using internal definitions that are local to the procedure
;; only the sqrt procedure is important to users

;; Method1
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       0.001))
  (define (improve guess x)
    (define (average x y)
      (/ (+ x y) 2))
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (sqrt-iter 1.0 x))

;; Method2 allow x to be a free variable in the internal definitions
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
(sqrt 1.0)
(sqrt 100)
(sqrt 3.0)
(sqrt 100000000000000)
(sqrt (+ 1 5))

;; -------------------------------------------------------------------
;; section 1.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;; factorial ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursive version
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)

;; iterative version
(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* product counter)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)

;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regard inc and dec as primitive
;; wrong, can't use procedure to present inc and dec
(define (inc a)
  (+ a 1))
(define (dec a)
  (- a 1))
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
(+ 4 5)

;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)
(define (f n) (A 0 n))
(f 4)
(define (g n) (A 1 n))
(g 4)
(define (h n) (A 2 n))
(h 4)
(define (k n) (* 5 n n))
(k 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fibonacci ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Recursion example, using recursive
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)

;; using iterative
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 n))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(count-change 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 1.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))
(define (report-prime elapsed-time)
  (display "***")
  
  (display elapsed-time)
  #t)
(time-prime-test 9041)
