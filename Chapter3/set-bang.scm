(define x (cons 14 '()))
(define y x)
(set! x (cons 42 '()))
(define fourteen (car y))