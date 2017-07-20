;; evaluate the following expression, and then debug in the REPL.
(load "Sec_4_1_meval_readfile.scm")
;; in this version, has a line of (list 'map map) in the primitive-procedures.

;; debug result:
1 ]=> 
;Loading "Sec_4_1_meval_readfile.scm"... done
;Value: announce-output

1 ]=> (apply-in-underlying-scheme map (list square '(1 2 3)))
 ;Value 16: (1 4 9)

1 ]=> 

;;; M-EVAL input:
  C-c C-c;Quit!

1 ]=> (define input (read))
(map square '(1 2 3 4))

;Value: input

1 ]=> input

;Value 11: (map square (quote (1 2 3 4)))

1 ]=> (eval input the-global-environment)

;The object (primitive #[compound-procedure 12]) is not applicable.
;To continue, call RESTART with an option number:

3 error> (define input-operator (eval (operator input) the-global-environment))

;Value: input-operator

3 error> input-operator

;Value 13: (primitive #[compiled-procedure 14 ("list" #x6f) #xf #x10230c3])

3 error> (define input-operands (list-of-values (operands input) the-global-environment))

;Value: input-operands

3 error> input-operands

;Value 15: ((primitive #[compound-procedure 12]) (1 2 3 4))

4 error> (apply-primitive input-operator input-operands)

;The object (primitive #[compound-procedure 12]) is not applicable.
;To continue, call RESTART with an option number:

5 error> (apply-in-underlying-scheme (primitive-implementation input-operator) input-operands)

;The object (primitive #[compound-procedure 12]) is not applicable.
;To continue, call RESTART with an option number:

6 error> (primitive-implementation input-operator)

;Value 14: #[compiled-procedure 14 ("list" #x6f) #xf #x10230c3]

7 error> (define input-operands-simply (cons (cadar input-operands) (cdr input-operands))
)

;Value: input-operands-simply

7 error> input-operands-simply

;Value 16: (#[compound-procedure 12] (1 2 3 4))

7 error> (apply-in-underlying-scheme (primitive-implementation input-operator) input-operands-simply)

;Value 17: (1 4 9 16)
