(load "Sec_4_1_meval.scm")

(driver-loop)

(load "Sec_4_1_meval_readfile.scm")
(driver-loop)

;; result: change the definition of append only for test.
;; ;;; M-EVAL input:
;; (load "test_input.scm")

;; ;;; M-EVAL value:
;; ok

;; ;;; M-EVAL input:
;; (append '(1 2) '(3 4))

;; ;;; M-EVAL value:
;; (1 2)

