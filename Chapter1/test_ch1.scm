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

;; case analysis
;; cond (special form)
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
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

;; logic composition operations:and, or, not
(define (>= x y)
  (or (> x y) (= x y)))
(define (>= x y)
  (not (< x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.1
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

;; Exercise 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
;; max-larger2返回两个较大的数的平方和
(define (max-larger2 a b c)
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
(max-larger2 3 4 5)
(max-larger2 3 5 4)
(max-larger2 4 3 5)
(max-larger2 4 5 3)
(max-larger2 5 3 4)
(max-larger2 5 4 3)

;; Exercise 1.3 sicp.org answer
(define (sum-square-max a b c)
  (define x (if (> a b) a b))
  (define y (if (< a b) a b))
  (define z (if (> y c) y c))
  (+ (* x x) (* z z)))
(sum-square-max 3 4 5)
(sum-square-max 3 5 4)
(sum-square-max 4 3 5)
(sum-square-max 4 5 3)
(sum-square-max 5 3 4)
(sum-square-max 5 4 3)

;; Exercise 1.4
;; return a+abs(b)
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -)
   a b))

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
;; 执行时，进入死循环，说明解释器采用的是应用序(applicative-order)
;; 如果解释器采用的是正则序,则运算的结果是０，不会进入无限循环
;; 实验结果显示，此解释器采用的是应用序
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square Roots by Newton's Method(Alexandria Method)
;; 采用的是迭代法(iterative)
;; 可以看到Scheme不需要专门的迭代/循环(iterative/looping)结构来让计算机重复执行
;; 某些指令，仅仅用函数调用就能完成迭代
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
;;初始值总是取1.0
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.6
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
;; 执行结果显示，程序未能正确执行，原因在于进入了一个无限递归
;; if是一个special form，有其特定的执行规则，首先判断predicate是否成立，如果成立
;; 则执行then-clause，否则执行else-clause，而这里的new-if虽然是由cond定义的
;; cond是一个特殊形式，但是new-if本身是一个普通过程，所以执行时需要按照应用式
;; (applicative order)进行代换，applicative order要求所有的operand都要计算
;; 出来，才能将operator应用到相应的operand上，这就造成了每次执行new-if时，都会执
;; 行sqrt-iter，从而造成了无限递归

;; Exercise 1.7
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
(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

;; Exercise 1.8
;; Cube Root by Newton's Method
(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x)
                 x)))
(define (cube-root x)
  (cube-root-iter 1.0 x))
(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))
(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.01))
(define (cube x)
  (* x x x))
(cube-root 27)
(cube-root 1000)
(cube-root 64)
(cube-root 1)
(cube-root -27)
(cube-root -1000)
(cube-root -64)
(cube-root -1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using internal definitions that are local to the procedure
;; only the sqrt procedure is important to users

;; Method1
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       0.001))
  (define (improve guess x)
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
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2

;; computing factorials using recursive
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

;; computing factorials using iterative
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter counter result n)
  (if (> counter n)
      result
      (fact-iter (+ counter 1)
                 (* result counter)
                 n)))

;; Exercise 1.10
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

;; Tree Recursion
;; Example Fibonacci
;; using recursive 
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
(define (fib-iter b a count)
  (if (= count 0)
      b
      (fib-iter a (+ a b) (- count 1))))
(define (fib n)
  (fib-iter 0 1 n))

;; Example: Counting change
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

;; Exercise 1.11
;; if n < 3, f(n) = n, n >= 3, f(n) = f(n-1) + f(n+2) + f(n+3)
;; Method1 Tree Recursive
(define (fun1-11 n)
  (if (< n 3)
      n
      (+ (fun1-11 (- n 1))
         (* 2 (fun1-11 (- n 2)))
         (* 3 (fun1-11 (- n 3)))
         )))
(fun1-11 0)
(fun1-11 1)
(fun1-11 2)
(fun1-11 3)
(fun1-11 4)
(fun1-11 5)

;; Method2 Iterative
a->n-1, b->n-2, c->n-3
(define (fun1-11-iter c b a count)
  (if (= count 0)
      c
      (fun1-11-iter b a (+ a (* b 2) (* c 3)) (- count 1))))
(define (fun1-11 n)
  (fun1-11-iter 0 1 2 n))

;; sicp.org answer
;; 没有上面的解法简洁
(define (fun1-11-iter c b a count)
  (if (< count 3)
      a
      (fun1-11-iter b a (+ a (* b 2) (* c 3)) (- count 1))))
(define (fun1-11 n)
  (if (< n 3)
      n
      (fun1-11-iter 0 1 2 n)))

;; Exercise 1.12
(define (pascal row col)
  (cond ((> col row) 'error)
        ((= row 1) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))
(pascal 1 1)
(pascal 2 1)
(pascal 2 2)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)
(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)col 
(pascal 5 5)
(pascal 5 6)

;; Exponentiation
;; recursive
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(expt 4 0)
(expt 4 1)
(expt 4 2)
(expt 4 3)
(expt 4 4)
(expt 4 5)

;; iterative
(define (expt b n)
  (expt-iter 1 b n))
(define (expt-iter result b count)
  (if (= count 0)
      result
      (expt-iter (* result b) b (- count 1))))

;; fast-expt
;; recursive
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(fast-expt 4 0)
(fast-expt 4 1)
(fast-expt 4 2)
(fast-expt 4 3)
(fast-expt 4 4)
(fast-expt 4 5)

;; iterative(Exercise 1.16)
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))
(define (fast-expt b n)
  (fast-expt-iter 1 b n))

;; Exercise 1.17
;; using add to implement multiply
(define (* a b)
  (if (= b 1)
      0
      (+ a (* a (- b 1)))))
(* 1 1)
(* 2 2)
(* 3 3)
(* 4 4)
(* 5 5)
(* 6 6)
(* 7 7)
(* 8 8)
(* 9 9)
(* 10 11)

;; Recursive
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))
(define (double x)
  (+ x x))
(define (halve x)
  (if (even? x)
      (/ x 2)
      ('error)))

;; Iterative (Exercise 1.18)
(define (* a b)
  (multi-iter 0 a b))
(define (multi-iter result a b)
  (cond ((= b 0) result)
        ((even? b) (multi-iter result (double a) (halve b)))
        (else (multi-iter (+ result a) a (- b 1)))))

;; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p p) (* q q))
                                 (+ (* q q) (* 2 (* p q)))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;; 注意到上面将(* 2 p q)改写成(* 2 (* p q))
;; 原因是这里用的是题1.18中所写的*，这个过程仅仅支持两个操作数的乘法

;; Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 40 206)
(gcd 206 40)

;; Testing for Primality
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? test-divisor n)
  (= (remainder n test-divisor) 0))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 1)
(prime? 2)
(prime? 3)
(prime? 4)
(prime? 5)
(prime? 6)
(prime? 7)
(prime? 8)
(prime? 9)
(prime? 10)
(prime? 11)
(prime? 17)
(prime? 1289)
(prime? 9001)
(prime? 9040)
(prime? 9041)

;; The Fermat test (Probabilistic methods)
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))
;; 注意下面的过程定义中，将n看成了一个在fermat-test过程内的自由变量
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
(fast-prime? 1 100)
;; 判断1时有问题，因为此时用了(random 0)，而random要求其参数范围为1~4294967087
(fast-prime? 2 100)
(fast-prime? 3 100)
(fast-prime? 4 100)
(fast-prime? 5 100)
(fast-prime? 6 100)
(fast-prime? 7 100)
(fast-prime? 8 100)
(fast-prime? 9 100)
(fast-prime? 10 100)
(fast-prime? 11 100)
(fast-prime? 17 100)
(fast-prime? 1289 100)
(fast-prime? 9001 100)
(fast-prime? 9040 100)
(fast-prime? 9041 100)

;; test random
(random 4294967087)
(random 4294967088)	;; error

;; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22
(define (timed-prime-test n)
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

(define (search-for-primes n)
  (if (timed-prime-test n)
      'finish
      (search-for-primes (+ n 2))))
(search-for-primes 1001)
(search-for-primes 10001)
(search-for-primes 100001)
(search-for-primes 1000001)
(search-for-primes 10000001)
(search-for-primes 100000001)
(search-for-primes 1000000001)
(search-for-primes 10000000001)
(search-for-primes 100000000001)
(search-for-primes 1000000000001) 

;; Exercise 1.23
(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; Exercise 1.24
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))
      #f))

;; Exercise 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3

;; 3 procedures that share a common underlying pattern
(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (+ a 1) b))))
(sum-int 0 10)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(define (cube x)
  (* x x x))
(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))
(* 8 (pi-sum 1 1000))

;; using high order procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)
(define (sum-int a b)
  (sum identity a inc b))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; using the high order function sum to implement integral
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Exercise 1.29
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (* 2 (sum f (+ a (* h 2)) next (- b (* h 2))))
        (* 4 (sum f (+ a h) next (- b h)))
        (f b))))
(integral-simpson cube 0 1.0 1000)
(integral-simpson cube 0 1.0 10000)

;; Exercise 1.30
;; iterative implementation of sum
(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

;; Exercise 1.31
;; recursive
(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a) (product term (next a) next b))))
(product identity 1 inc 5)

;; iterative
(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1.0))

(define (pi-term x)
  (* (/ (- x 1) x)
     (/ (+ x 1) x)))
(define (inc2 x)
  (+ x 2))
(* 4 (product pi-term 3.0 inc2 1000.0))

;; Exercise 1.32
;; higher layer abstraction
;; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0.0 term a next b))
(define (product term a next b)
  (accumulate * 1.0 term a next b))

;; iterative
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combiner result (term a)))))
  (accumulate-iter a null-value))

;; Exercise 1.33
;; Recursive
(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a)
                    (term a)
                    null-value)
                (filtered-accumulate filter?
                                        combiner
                                        null-value
                                        term
                                        (next a)
                                        next
                                        b))))
(define (add-prime-square a b)
  (filtered-accumulate prime? + 0 square a inc b))
(add-prime-square 2 13)

(define (product-relatively-prime n)
  (define (gcd-filter? x)
    (= (gcd n x) 1))
  (filtered-accumulate gcd-filter? * 1 identity 1 inc n))
(product-relatively-prime 13)

;; Finding roots of equations by the half-interval method
(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (/ (+ neg-point pos-point) 2)))
    (cond ((close-enough? neg-point pos-point)
           midpoint)
          ((< (f midpoint) 0)
           (search f midpoint pos-point))
          ((> (f midpoint) 0)
           (search f neg-point midpoint))
          (else
           midpoint))))
(search (lambda (x) (- (* x x x) (* 2 x) 3))
        1.0
        2.0)

;; 注意下面的函数用到了error函数
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0)

;; Finding fixed points of functions
(define tolerance 0.00001)
;; Iterative
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(define (average x y)
  (/ (+ x y) 2))
(sqrt 1.0)
(sqrt 2.0)
(sqrt 3.0)
(sqrt 4.0)
(sqrt 5.0)

;; Exercise 1.35
(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
             1.0)

;; Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (let ((next (f guess)))
      (display n)
      (display " ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ n 1)))))
  (try first-guess 1))

;; without using average damping
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)
;; using average damping
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
             2.0)

;; Exercise 1.37
;; Recursive
(define (cont-frac n d k)
  (define (cont-frac-recursive i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recursive (+ i 1))))))
  (cont-frac-recursive 1.0))
;; Iterative
(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (< i 1)
        result
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)

;; Exercise 1.38
(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder i 3) 2)
                 (+ 2 (* 2 (/ (- i 2) 3)))
                 1))
           1000)

;; Exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))
(tan-cf (/ 3.14 4) 10000)

;; procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))
((average-damp square) 10)
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt 1.0)
(sqrt 2.0)
(sqrt 3.0)
(sqrt 4.0)
(sqrt 5.0)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(cube-root 1.0)
(cube-root 2.0)
(cube-root 3.0)
(cube-root 4.0)
(cube-root 8.0)
(cube-root 27.0)

;; Newton's Method (fixed point)
;; derivative
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (cube x) (* x x x))
(deriv cube)
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(newton-method (lambda (x) (- (square x) 2.0))
               1.0)

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))
                 1.0))
(sqrt 2.0)

;; higher layer abstraction
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            4.0))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))
(newton-method (cubic 1 0 0) 0)		;; 收敛于0这个零点
(newton-method (cubic 1 0 0) -4.0)	;; 收敛于－1这个零点

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x)
  (+ x 1))
(((double (double double)) inc) 5)

;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
((compose square inc) 6)

;; Exercise 1.43
(define (repeated f n)
  (define (repeated-iter i result)
    (if (> i n)
        result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 (lambda (x) x)))
((repeated square 4) 5)

;; Exercise 1.44
(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0))))
(define (smooth-ntimes f n)
  ((repeated smooth n) f))

;; Exercise 1.45
(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y))))
               1.0))
(fourth-root 1.0)
(fourth-root 2.0)
(fourth-root 3.0)

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x (* y y y y))))
               1.0))
(fifth-root 1.0)
(fifth-root 2.0)
(fifth-root 3.0)

(define (nth-root x n)
  (fixed-point ((repeated average-damp (if (even? n)
                                           (/ n 2)
                                           (/ (- n 1) 2)))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
(nth-root 2.0 0) ;; 由于前面编写expt函数时没有考虑负指数的情况，所以此时存在问题
(nth-root 2.0 1)
(nth-root 2.0 2)
(nth-root 2.0 3)
(nth-root 2.0 4)
(nth-root 2.0 5)
(nth-root 2.0 6)

;; Exercise 1.46
(define (iterative-improve good-enough? improve-guess initial-value)
  (let ((next-value (improve-guess initial-value)))
    (if (good-enough? initial-value next-value)
        next-value
        (iterative-improve good-enough? improve-guess next-value))))

(define (good-enough? initial-value next-value)
  (< (abs (- initial-value next-value)) 0.00001))

(define (sqrt x)
  (iterative-improve good-enough?
                     (lambda (y) (/ (+ y (/ x y)) 2))
                     1.0))
(define (fixed-point f first-guess)
  (iterative-improve good-enough?
                     f
                     first-guess))
