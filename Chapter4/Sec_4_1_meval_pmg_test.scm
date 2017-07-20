(load "Sec_4_1_meval_pmg.scm")

(driver-loop)

;; test result
;; 1 ]=> (load "Sec_4_1_meval_pmg_test.scm")

;; ;Loading "Sec_4_1_meval_pmg_test.scm"...
;; ;  Loading "Sec_4_1_meval_pmg.scm"... done


;; ;;; M-EVAL input:
;; (load "primitive_added_by_load.scm")

;; ;;; M-EVAL value:
;; file-read-complete

;; ;;; M-EVAL input:
;; (append '(1 2) '(3 4))

;; ;;; M-EVAL value:
;; (1 2 3 4)

;; ;;; M-EVAL input:
;; (map square '(1 2 3 4 5 6))

;; ;;; M-EVAL value:
;; (1 4 9 16 25 36)

;; ;;; M-EVAL input:
;; (map (lambda (x) (+ x 1)) '(1 2 3 4 5 6))

;; ;;; M-EVAL value:
;; (2 3 4 5 6 7)

;; ;;; M-EVAL input:
