(define f
  (let ((num 1))
    (lambda (x)
      (set! num (* num x))
      num)))

(+ (f 0) (f 1))
;; will return 0 if the arguments are evaluated from left to right,
;; will return 1 if the arguments are evaluated from right to left.