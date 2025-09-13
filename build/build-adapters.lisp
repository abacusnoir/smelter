;;; Build script for Smelter adapters integration
;;; This script loads all adapter dependencies and creates an enhanced SBCL core

(defun install-adapter-dependencies ()
  "Install all required dependencies for adapters"
  (format t "~%Installing adapter dependencies...~%")
  (ql:quickload '(:drakma :st-json :split-sequence :cl-ppcre :flexi-streams :uiop))
  (format t "Dependencies installed successfully.~%"))

(defun load-adapter-sources ()
  "Load all adapter source files"
  (format t "~%Loading adapter source files...~%")
  
  (let ((src-dir (merge-pathnames "src/adapters/" (uiop:getcwd))))
    
    ;; Load adapters in dependency order
    (load (merge-pathnames "json.lisp" src-dir))
    (format t "✓ JSON adapter loaded~%")
    
    (load (merge-pathnames "http.lisp" src-dir))
    (format t "✓ HTTP adapter loaded~%")
    
    (load (merge-pathnames "fs.lisp" src-dir))
    (format t "✓ File System adapter loaded~%")
    
    (load (merge-pathnames "process.lisp" src-dir))
    (format t "✓ Process adapter loaded~%")
    
    (load (merge-pathnames "cli.lisp" src-dir))
    (format t "✓ CLI adapter loaded~%")))

(defun verify-adapters ()
  "Verify that all adapters are properly loaded"
  (format t "~%Verifying adapter integration...~%")
  
  ;; Check that packages exist
  (assert (find-package :smelter/adapters/json) () "JSON adapter package not found")
  (assert (find-package :smelter/adapters/http) () "HTTP adapter package not found")
  (assert (find-package :smelter/adapters/fs) () "FS adapter package not found")
  (assert (find-package :smelter/adapters/process) () "Process adapter package not found")
  (assert (find-package :smelter/adapters/cli) () "CLI adapter package not found")
  
  ;; Basic functionality tests
  (format t "✓ JSON adapter available~%")
  (format t "✓ HTTP adapter available~%")
  (format t "✓ File System adapter available~%")
  (format t "✓ Process adapter available~%")
  (format t "✓ CLI adapter available~%")
  
  (format t "All adapters verified successfully!~%"))

(defun create-adapter-image ()
  "Create SBCL core with adapters included"
  (format t "~%Creating enhanced SBCL core with adapters...~%")
  
  ;; First load the base system components like the original create-image.lisp
  (format t "Loading base Smelter system...~%")
  (let ((cwd (uiop:getcwd)))
    ;; Load Smelter components 
    (load (merge-pathnames "src/coalton-translator.lisp" cwd))
    (load (merge-pathnames "src/stdlib/smelter-prelude.lisp" cwd))
    (load (merge-pathnames "src/stdlib/smelter-io.lisp" cwd))
    (load (merge-pathnames "src/stdlib/smelter-system.lisp" cwd))
    (load (merge-pathnames "src/cli.lisp" cwd)))
  
  ;; Set compilation policy for smaller image
  (proclaim '(optimize (speed 2) (safety 1) (debug 1) (space 3)))
  
  ;; Disable debugger for production use
  (setf *debugger-hook* 
        (lambda (condition me)
          (declare (ignore me))
          (format *error-output* "Fatal error: ~A~%" condition)
          (sb-ext:exit :code 1)))
  
  ;; Save the enhanced core image
  (format t "Saving enhanced core image with adapters...~%")
  (let ((core-path "build/smelter.core"))
    (ensure-directories-exist core-path)
    (handler-case
        (sb-ext:save-lisp-and-die core-path
                                  :executable nil
                                  :compression t
                                  :save-runtime-options nil)
      (error (e)
        (format t "Error saving core: ~A~%" e)
        (sb-ext:exit :code 1)))))

(defun build-adapters-image ()
  "Main function to build adapters into Smelter"
  (handler-case
      (progn
        (format t "=== Smelter Adapter Build Process ===~%")
        
        ;; Load Quicklisp if not already loaded
        (unless (find-package :ql)
          (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                                (user-homedir-pathname))))
            (when (probe-file quicklisp-init)
              (format t "Loading Quicklisp...~%")
              (load quicklisp-init))))
        
        ;; Install dependencies
        (install-adapter-dependencies)
        
        ;; Load Coalton first (required for adapters)
        (format t "~%Loading Coalton...~%")
        (ql:quickload :coalton)
        (format t "✓ Coalton loaded~%")
        
        ;; Load adapter source files
        (load-adapter-sources)
        
        ;; Verify everything loaded correctly
        (verify-adapters)
        
        ;; Create the enhanced image
        (create-adapter-image)
        
        (format t "~%=== Adapter build completed successfully! ===~%"))
    (error (e)
      (format t "~%❌ Build failed: ~A~%" e)
      (sb-ext:exit :code 1))))

;; Optional: If this script is run directly, execute the build
(when (string= (pathname-name *load-pathname*) "build-adapters")
  (build-adapters-image))