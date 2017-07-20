;; int the M-EVAL REPL, type (load "primitive_added_by_load.scm")

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

