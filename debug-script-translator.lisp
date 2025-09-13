(load "src/coalton-translator.lisp")

(defun test-script-translation (content)
  "Test what the script translator outputs"
  (format t "Input:~%~A~%~%" content)
  (format t "Output:~%~A~%" 
          (smelter.translator:translate-pure-coalton content :for-repl nil))
  (terpri))

;; Test a simple script
(test-script-translation "(declare greet (String -> String))
(define (greet name) (concatenate \"Hello, \" name))
(define main (greet \"World\"))")