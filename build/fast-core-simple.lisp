;;;; build/fast-core-simple.lisp
;;;; Ultra-minimal, fast-starting Smelter core
;;;; Goal: <66ms startup time

(in-package :cl-user)

;;; Ultra-minimal arithmetic-only build for testing absolute minimum startup time
(defun arithmetic-only-eval (expr-string)
  "Ultra-minimal evaluator - only arithmetic, no Coalton"
  (handler-case
      (let* ((expr (read-from-string expr-string))
             (result (eval expr)))  ; Direct CL eval!
        (format t "~A~%" result)
        0)
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      1)))

(defun arithmetic-only-main ()
  "Main for arithmetic-only testing"
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond
      ((and (string= (first args) "eval") (second args))
       (sb-ext:exit :code (arithmetic-only-eval (second args))))
      ((string= (first args) "version")
       (format t "Smelter v0.1.0 (minimal test build)~%")
       (sb-ext:exit :code 0))
      (t
       (format *error-output* "Usage: smt-minimal eval '(+ 1 2)' | version~%")
       (sb-ext:exit :code 1)))))

(defun configure-sbcl-for-speed ()
  "Configure SBCL for maximum startup speed"
  ;; Disable all debugging for production
  (sb-ext:disable-debugger)
  
  ;; Maximum speed optimization
  (proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 2)))
  
  ;; Disable verbose loading
  (setf *load-verbose* nil)
  (setf *compile-verbose* nil)
  (setf *load-print* nil)
  (setf *compile-print* nil)
  
  ;; Minimal GC
  (when (fboundp 'sb-ext:gc)
    (sb-ext:gc :full t)))

(defun build-minimal-test-image ()
  "Build ultra-minimal image for testing baseline performance"
  (format t "Building ultra-minimal Smelter test image...~%")
  (configure-sbcl-for-speed)
  (sb-ext:save-lisp-and-die
   "smt-minimal"
   :toplevel #'arithmetic-only-main
   :executable t
   :compression t))