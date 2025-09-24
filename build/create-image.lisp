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
      
      ;; Load adapter dependencies
      (format t "Loading adapter dependencies...~%")
      (ql:quickload :st-json :silent t)
      (ql:quickload :split-sequence :silent t)
      (ql:quickload :drakma :silent t)
      (ql:quickload :cl-csv :silent t)
      
      ;; Verify Coalton is loaded
      (unless (find-package :coalton)
        (error "Coalton package not found after loading"))
      
      ;; Load coalton-prelude for arithmetic operators
      (unless (find-package :coalton-prelude)
        (error "Coalton-prelude package not found"))
      
      ;; Set up coalton-user package with proper prelude imports
      (format t "Setting up coalton-user package...~%")
      (unless (find-package :coalton-user)
        (defpackage :coalton-user
          (:use 
           :cl
           :coalton-prelude)))
      
      (format t "Coalton loaded successfully.~%")
      
      ;; Load Smelter components directly  
      (format t "Loading Smelter components...~%")
      (let ((cwd (uiop:getcwd)))
        ;; Skip preprocessor - using simplified approach
        (load (merge-pathnames "src/coalton-translator.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-prelude.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-io.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-system.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-file.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-http.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-json.lisp" cwd))
        
        ;; Load CSV module with qualified types
        (format t "Loading CSV module...~%")
        (load (merge-pathnames "src/stdlib/smelter-csv.lisp" cwd))
        
        ;; Load test module
        (format t "Loading test module...~%")
        (load (merge-pathnames "src/stdlib/smelter-test.lisp" cwd))

        ;; Load datetime module
        (format t "Loading datetime module...~%")
        (load (merge-pathnames "src/stdlib/smelter-datetime.lisp" cwd))
        
        ;; Load just JSON adapter for now to test the test library
        (format t "Loading Smelter JSON adapter...~%")
        (load (merge-pathnames "src/adapters/json.lisp" cwd))
        ;; Other adapters disabled temporarily
        ;; (load (merge-pathnames "src/adapters/cli.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/process.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/http.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/fs.lisp" cwd))
        
        (load (merge-pathnames "src/cli.lisp" cwd)))
      
      ;; Verify all packages are loaded
      (unless (find-package :smelter.translator)
        (error "Smelter translator package not found"))
      (unless (find-package :smelter.stdlib.prelude)
        (error "Smelter stdlib.prelude package not found"))
      (unless (find-package :smelter.stdlib.io)
        (error "Smelter stdlib.io package not found"))
      (unless (find-package :smelter.stdlib.system)
        (error "Smelter stdlib.system package not found"))
      (unless (find-package :smelter.stdlib.file)
        (error "Smelter stdlib.file package not found"))
      (unless (find-package :smelter)
        (error "Smelter main package not found"))
      
      ;; Verify CSV package 
      (unless (find-package :smelter.stdlib.csv)
        (error "Smelter CSV package not found"))
      
      ;; Verify test package
      (unless (find-package :smelter.stdlib.test)
        (error "Smelter test package not found"))

      ;; Verify datetime package
      (unless (find-package :smelter.stdlib.datetime)
        (error "Smelter datetime package not found"))
      
      ;; Verify JSON adapter package
      (unless (find-package :smelter/adapters/json)
        (error "Smelter JSON adapter package not found"))
      ;; Other adapter packages verification disabled temporarily
      ;; (unless (find-package :smelter/adapters/cli)
      ;;   (error "Smelter CLI adapter package not found"))
      ;; (unless (find-package :smelter/adapters/process)
      ;;   (error "Smelter Process adapter package not found"))
      ;; (unless (find-package :smelter/adapters/http)
      ;;   (error "Smelter HTTP adapter package not found"))
      ;; (unless (find-package :smelter/adapters/fs)
      ;;   (error "Smelter FS adapter package not found"))
      
      (format t "Smelter system loaded successfully.~%")
      
      ;; Test basic functionality
      (format t "Testing Coalton functionality...~%")
      (format t "Coalton test passed: basic loading successful~%"))
  
  (error (e)
    (format t "Error loading systems: ~A~%" e)
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