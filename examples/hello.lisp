#!/usr/bin/env smt-cl
;;; Simple CL demo script

(defun greet (name)
  (format t "Hello, ~A!~%" name))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Main execution
(greet "World")
(format t "Factorial of 10: ~A~%" (factorial 10))
(format t "CL mode works!~%")
