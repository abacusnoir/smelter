;;;; Smelter Image Builder
;;;; Creates SBCL core with Coalton pre-loaded for fast startup

(format t "~%=== Smelter Image Builder ===~%")
(format t "Building SBCL core with embedded Coalton...~%~%")

;;; Ensure we have a clean environment
(setf *print-case* :downcase)

;;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                      (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (format t "Loading Quicklisp...~%")
    (load quicklisp-init)
    (format t "Quicklisp loaded.~%"))
  (unless (find-package :ql)
    (error "Quicklisp not found. Please install Quicklisp first.")))

;;; Load Coalton and dependencies
(format t "Loading Coalton...~%")
(handler-case
    (progn
      ;; Load Coalton system
      (ql:quickload :coalton :silent t)
      
      ;; Verify Coalton is loaded
      (unless (find-package :coalton)
        (error "Coalton package not found after loading"))
      
      ;; Load coalton-prelude for arithmetic operators
      (unless (find-package :coalton-prelude)
        (error "Coalton-prelude package not found"))
      
      (format t "Coalton loaded successfully.~%")
      
      ;; Test basic functionality
      (format t "Testing Coalton functionality...~%")
      (eval (read-from-string "
        (defpackage #:smelter-test
          (:use #:coalton #:coalton-prelude))
        
        (in-package #:smelter-test)
        
        (coalton-toplevel
          (declare test-add (Integer -> Integer -> Integer))
          (define (test-add x y) (+ x y)))"))
      
      (let ((result (funcall (find-symbol "TEST-ADD" :smelter-test) 2 3)))
        (unless (= result 5)
          (error "Coalton test failed: expected 5, got ~A" result))
        (format t "Coalton test passed: 2 + 3 = ~A~%" result)))
  
  (error (e)
    (format t "Error loading Coalton: ~A~%" e)
    (sb-ext:exit :code 1)))

;;; Optimize the image
(format t "Optimizing image...~%")

;; Set compilation policy for smaller image
(proclaim '(optimize (speed 2) (safety 1) (debug 1) (space 3)))

;; Clean up temporary packages
(when (find-package :smelter-test)
  (delete-package :smelter-test))

;; Clear some caches to reduce image size
(handler-case
    (progn
      ;; Clear ASDF cache if available
      (when (find-package :asdf)
        (let ((clear-cache (find-symbol "CLEAR-CACHE" :asdf)))
          (when clear-cache
            (funcall clear-cache))))
      
      ;; Clear Quicklisp temporary data
      (when (find-package :ql)
        (let ((clear-dist-cache (find-symbol "CLEAR-DIST-CACHE" :ql)))
          (when clear-dist-cache
            (handler-case (funcall clear-dist-cache)
              (error () nil))))))
  (error () nil)) ; Ignore errors in cleanup

;; Disable debugger for production use
(setf *debugger-hook* 
      (lambda (condition me)
        (declare (ignore me))
        (format *error-output* "Fatal error: ~A~%" condition)
        (sb-ext:exit :code 1)))

;;; Save the core image
(format t "Saving core image...~%")

(let ((core-path "build/smelter.core"))
  (ensure-directories-exist core-path)
  
  (handler-case
      (progn
        (sb-ext:save-lisp-and-die core-path
                                  :executable nil
                                  :compression t
                                  :save-runtime-options nil))
    (error (e)
      (format t "Error saving core: ~A~%" e)
      (sb-ext:exit :code 1))))

;; This point should never be reached
(format t "Error: save-lisp-and-die returned unexpectedly~%")
(sb-ext:exit :code 1)