(define (make-accumulator initial)
  (lambda (x)
    (set! initial (+ initial x))
    initial))
(define A (make-accumulator 5))
(A 10)
(A 10)
(A 100)
