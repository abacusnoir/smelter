(defpackage #:smelter.stdlib.test
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:io #:smelter.stdlib.io))
  (:export
   #:TestResult #:TestPass #:TestFail
   #:test-case
   #:run-test-suite))

(in-package #:smelter.stdlib.test)

(coalton-toplevel
  (define-type TestResult
    "Represents the outcome of a single test."
    (TestPass String)
    (TestFail String String))

  (declare test-case (String -> (Unit -> Boolean) -> (Unit -> TestResult)))
  (define (test-case name test-fn)
    "Define a single test case. Takes a name and a function that returns a boolean.
    Returns a new function that can be run as part of a test suite."
    (fn ()
      (if (test-fn)
          (TestPass name)
          (TestFail name "Assertion failed"))))

  (declare print-result (TestResult -> Unit))
  (define (print-result result)
    "Print the result of a single test case."
    (match result
      ((TestPass name)
       (io:io-println (mconcat (make-list "✅ " name))))
      ((TestFail name reason)
       (io:io-println (mconcat (make-list "❌ " name " - " reason))))))

  (declare count-passed ((List TestResult) -> UFix))
  (define (count-passed results)
    "Count the number of passing tests in a list of results."
    (fold (fn (acc result)
            (match result
              ((TestPass _) (+ 1 acc))
              ((TestFail _ _) acc)))
          0
          results))

  (declare run-test-suite (String -> (List (Unit -> TestResult)) -> Unit))
  (define (run-test-suite title tests)
    "Run a list of test-case functions and print a summary report."
    (let ((results (map (fn (test) (test)) tests)))
      (let ((passed (count-passed results))
            (total (length results)))
        (progn
          (io:io-println (mconcat (make-list "=== " title " ===")))
          (map print-result results)
          (io:io-println "")
          (io:io-println (mconcat (make-list "Summary: " (into passed) "/" (into total) " passed.")))
          (if (== passed total)
              (io:io-println "✅ All tests passed!")
              (progn
                (io:io-println "❌ Some tests failed!")
                (lisp Unit () (uiop:quit 1)))))))))