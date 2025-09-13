;;;; Debug the translator output
(load "src/coalton-translator.lisp")

(defun test-translation (expr)
  "Test what the translator outputs for a given expression"
  (format t "Input: ~A~%" expr)
  (format t "Output:~%~A~%" 
          (smelter.translator:translate-pure-coalton expr :for-repl t))
  (terpri))

;; Test various expressions
(test-translation "(+ 2 3)")
(test-translation "(map (fn (x) (* x 2)) (list 1 2 3))")
(test-translation "(fold + 0 (list 1 2 3 4))")