;;;; smelter.asd

(asdf:defsystem #:smelter
  :description "Industrial-strength typed scripting with Coalton"
  :author "Smelter Team"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/yourusername/smelter"
  :bug-tracker "https://github.com/yourusername/smelter/issues"
  :source-control (:git "https://github.com/yourusername/smelter.git")
  
  ;; Dependencies
  :depends-on (#:coalton
               #:uiop)
  
  ;; Components
  :components ((:module "src"
                :serial t
                :components
                ((:file "coalton-translator")
                 (:module "stdlib"
                  :components
                  ((:file "smelter-io")
                   (:file "smelter-system")))
                 (:file "cli"))))
  
  ;; Build configuration
  :build-operation "program-op"
  :build-pathname "smt"
  :entry-point "smelter:main"
  
  ;; Development dependencies for testing
  :in-order-to ((test-op (test-op #:smelter/tests))))

;; Test system
(asdf:defsystem #:smelter/tests
  :description "Test suite for Smelter"
  :depends-on (#:smelter
               #:fiveam)
  :components ((:module "test"
                :components
                ((:file "coalton-translator-tests")
                 (:file "smoke-tests"))))
  :perform (test-op (o s)
             (symbol-call :fiveam :run! :smelter-tests)))