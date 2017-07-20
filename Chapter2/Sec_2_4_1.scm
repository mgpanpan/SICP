;; Section 2.4.1
;; Illustriate that the same implementation of the procedures that use
;; the complex number package, (add-complex, sub-complex, mul-complex and
;; div-complex) will work with either of the two representation of complex
;; number.

;; wishful thinking
;; assume that procedures: make-from-real-imag, make-from-mag-ang,
;; real-part, imag-part, magnitude and angle are already constucted.
;; these are the constructors and selectors of the abstract data-complex number.
;; regard these procedures as the interface that the complex number package
;; provide to the client.
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

;; the implementation of the constructors:
;; make-from-real-imag, make-from-mag-ang
;; and the selectors: real-part, imag-part, magnitude and angle
;; depends on the representation methods of the complex number, different
;; representations lead to different implementations of the constructors
;; and selectors.

;; rectangular form
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a))
        (* r (sin a))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z))); should use two parameters version of atan

;; polar form
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x))) ; should use two parameters version of atan
(define (make-from-mag-ang r a) (cons r a))
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
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

