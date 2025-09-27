;;;
;;; build/create-image.lisp
;;;
;;; This script builds the Smelter executable using ASDF, the standard
;;; Common Lisp build system. This is the robust, correct way to build the
;;; system, ensuring all dependencies, including Coalton's runtime machinery,
;;; are loaded correctly.
;;;
(require 'asdf)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the Smelter system definition from the current directory
(asdf:load-asd (merge-pathnames "../smelter.asd" *load-truename*))

;; Load all of Smelter's dependencies via Quicklisp
(ql:quickload :smelter)

;;; =================================================================
;;; Build-Time Bootstrapping for Coalton ADT Pattern Matching
;;; This forces the compilation of dummy match expressions during the build,
;;; which in turn generates the necessary runtime helper functions
;;; for ADTs (Result, Tuple, List, Optional).
;;; =================================================================
(format t "--> Bootstrapping Coalton ADT pattern matching...~%")
(handler-case
    (progn
      (in-package #:coalton-user)
      (eval (read-from-string "
        (coalton:coalton-toplevel
          (declare bootstrap-result (Result Integer Integer -> Integer))
          (define (bootstrap-result x)
            (match x
              ((Ok n) n)
              ((Err _) 0)))
          (declare bootstrap-tuple (Tuple Integer String -> Integer))
          (define (bootstrap-tuple x)
            (match x
              ((Tuple n _) n)))
          (declare bootstrap-list (List Integer -> Integer))
          (define (bootstrap-list x)
            (match x
              ((Cons h _) h)
              ((Nil) 0)))
          (declare bootstrap-optional (Optional Integer -> Integer))
          (define (bootstrap-optional x)
            (match x
              ((Some n) n)
              ((None) 0))))"))
      (format t "--> Bootstrap complete.~%"))
  (error (e)
    (format *error-output* "ERROR during ADT bootstrap: ~A~%" e)
    (uiop:quit 1)))
(in-package #:cl-user)
;;; =================================================================

;; Save the final executable
(format t "--> Saving final executable 'smt'...~%")
(sb-ext:save-lisp-and-die "smt"
  :toplevel #'smelter:main
  :executable t
  :purify nil) ; :purify nil is crucial for preserving the dynamic compiler info

(uiop:quit)
