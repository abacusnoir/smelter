;;;; build/fast-core.lisp
;;;; Minimal, fast-starting Smelter core
;;;; Goal: <100ms startup time

(in-package :cl-user)

;;; ===========================================================================
;;; STAGE 1: Pre-compilation Script
;;; Run this BEFORE building the image
;;; ===========================================================================

(defun compile-all-sources ()
  "Pre-compile all Lisp sources to FASL for fast loading"
  (let ((files '("src/coalton-translator"
                 "src/cli"
                 "src/stdlib/smelter-prelude"
                 "src/stdlib/smelter-io"
                 "src/stdlib/smelter-system"
                 "src/stdlib/smelter-file"
                 "src/stdlib/smelter-http"
                 "src/stdlib/smelter-json"
                 "src/adapters/json")))
    (dolist (file files)
      (let ((source (concatenate 'string file ".lisp"))
            (fasl (concatenate 'string file ".fasl")))
        (when (probe-file source)
          (format t "Compiling ~A...~%" source)
          (compile-file source :output-file fasl))))))

;;; ===========================================================================
;;; STAGE 2: Minimal Core Loader
;;; This is what gets embedded in the binary
;;; ===========================================================================

(defpackage :smelter-fast
  (:use :cl)
  (:export #:main
           #:eval-minimal
           #:repl-minimal
           #:build-fast-image
           #:build-minimal-test-image
           #:compile-all-sources))

(in-package :smelter-fast)

;; Global flags for lazy loading
(defparameter *coalton-loaded* nil)
(defparameter *stdlib-loaded* nil)
(defparameter *http-loaded* nil)
(defparameter *json-loaded* nil)
(defparameter *adapters-loaded* nil)

(defun ensure-coalton ()
  "Load Coalton only when needed"
  (unless *coalton-loaded*
    ;; Load Coalton without Quicklisp - use ASDF directly if available
    (handler-case
        (progn
          (format t "Loading Coalton...~%")
          ;; Try to load from Quicklisp first
          (when (find-package :ql)
            (funcall (find-symbol "QUICKLOAD" :ql) :coalton :silent t))
          ;; Verify coalton-user package exists with arithmetic operators
          (unless (find-package :coalton-user)
            (defpackage :coalton-user
              (:use :cl)
              (:import-from :coalton-prelude
                            #:+ #:- #:* #:/
                            #:== #:< #:> #:<= #:>=)))
          (setf *coalton-loaded* t)
          (format t "Coalton loaded.~%"))
      (error (e)
        (format *error-output* "Warning: Could not load Coalton: ~A~%" e)
        ;; Fall back to basic arithmetic
        (unless (find-package :coalton-user)
          (defpackage :coalton-user (:use :cl)))
        (setf *coalton-loaded* :fallback)))))

(defun ensure-stdlib ()
  "Load standard library only when needed"
  (unless *stdlib-loaded*
    (ensure-coalton)
    (let ((cwd (or *default-pathname-defaults* (truename "."))))
      (format t "Loading Smelter stdlib...~%")
      (handler-case
          (progn
            ;; Load pre-compiled FASL files if available, otherwise source
            (dolist (component '("coalton-translator" "stdlib/smelter-prelude" 
                                "stdlib/smelter-io" "stdlib/smelter-system" 
                                "stdlib/smelter-file"))
              (let ((fasl (merge-pathnames (concatenate 'string "src/" component ".fasl") cwd))
                    (source (merge-pathnames (concatenate 'string "src/" component ".lisp") cwd)))
                (cond
                  ((probe-file fasl)
                   (load fasl :verbose nil :print nil))
                  ((probe-file source)
                   (load source :verbose nil :print nil))
                  (t (format t "Warning: Could not find ~A~%" component)))))
            (setf *stdlib-loaded* t)
            (format t "Stdlib loaded.~%"))
        (error (e)
          (format *error-output* "Warning: Could not load stdlib: ~A~%" e)
          (setf *stdlib-loaded* :partial))))))

(defun ensure-http ()
  "Load HTTP client only when needed"
  (unless *http-loaded*
    (ensure-stdlib)
    (handler-case
        (progn
          (format t "Loading HTTP support...~%")
          ;; Load Drakma dependency
          (when (find-package :ql)
            (funcall (find-symbol "QUICKLOAD" :ql) :drakma :silent t))
          ;; Load our HTTP wrapper
          (let* ((cwd (or *default-pathname-defaults* (truename ".")))
                 (fasl (merge-pathnames "src/stdlib/smelter-http.fasl" cwd))
                 (source (merge-pathnames "src/stdlib/smelter-http.lisp" cwd)))
            (cond
              ((probe-file fasl) (load fasl :verbose nil :print nil))
              ((probe-file source) (load source :verbose nil :print nil))))
          (setf *http-loaded* t)
          (format t "HTTP support loaded.~%"))
      (error (e)
        (format *error-output* "Warning: Could not load HTTP support: ~A~%" e)
        (setf *http-loaded* :failed)))))

(defun ensure-json ()
  "Load JSON parser only when needed"
  (unless *json-loaded*
    (ensure-stdlib)
    (handler-case
        (progn
          (format t "Loading JSON support...~%")
          ;; Load st-json dependency
          (when (find-package :ql)
            (funcall (find-symbol "QUICKLOAD" :ql) :st-json :silent t))
          ;; Load our JSON wrapper
          (let* ((cwd (or *default-pathname-defaults* (truename ".")))
                 (fasl (merge-pathnames "src/stdlib/smelter-json.fasl" cwd))
                 (source (merge-pathnames "src/stdlib/smelter-json.lisp" cwd)))
            (cond
              ((probe-file fasl) (load fasl :verbose nil :print nil))
              ((probe-file source) (load source :verbose nil :print nil))))
          (setf *json-loaded* t)
          (format t "JSON support loaded.~%"))
      (error (e)
        (format *error-output* "Warning: Could not load JSON support: ~A~%" e)
        (setf *json-loaded* :failed)))))

(defun ensure-adapters ()
  "Load adapters only when needed"
  (unless *adapters-loaded*
    (ensure-json)  ; JSON adapter depends on JSON support
    (handler-case
        (progn
          (format t "Loading adapters...~%")
          (let* ((cwd (or *default-pathname-defaults* (truename ".")))
                 (fasl (merge-pathnames "src/adapters/json.fasl" cwd))
                 (source (merge-pathnames "src/adapters/json.lisp" cwd)))
            (cond
              ((probe-file fasl) (load fasl :verbose nil :print nil))
              ((probe-file source) (load source :verbose nil :print nil))))
          (setf *adapters-loaded* t)
          (format t "Adapters loaded.~%"))
      (error (e)
        (format *error-output* "Warning: Could not load adapters: ~A~%" e)
        (setf *adapters-loaded* :failed)))))

;;; ===========================================================================
;;; STAGE 3: Fast Evaluation Path
;;; ===========================================================================

(defun detect-expression-needs (expr-string)
  "Analyze expression to determine what components are needed"
  (let ((lower-expr (string-downcase expr-string)))
    (list :needs-http (search "http" lower-expr)
          :needs-json (or (search "json" lower-expr) (search "parse" lower-expr))
          :needs-file (search "file" lower-expr)
          :needs-adapters (or (search "parse-json" lower-expr) 
                             (search "json-object" lower-expr)))))

(defun eval-minimal (expr-string)
  "Minimal evaluation - only load what's needed"
  (handler-case
      (let* ((expr (read-from-string expr-string))
             (needs (detect-expression-needs expr-string)))
        
        ;; Lazy load only what's needed
        (cond
          ((getf needs :needs-http) (ensure-http))
          ((getf needs :needs-json) (ensure-json))
          ((getf needs :needs-adapters) (ensure-adapters))
          ((getf needs :needs-file) (ensure-stdlib))
          ;; For simple arithmetic, just ensure basic Coalton
          (t (ensure-coalton)))
        
        ;; Evaluate the expression
        (let* ((*package* (find-package :coalton-user))
               ;; Try Coalton syntax first, fall back to regular Lisp
               (result (handler-case
                           (if (eq *coalton-loaded* :fallback)
                               ;; Direct Lisp evaluation for fallback
                               (eval expr)
                               ;; Coalton evaluation
                               (let ((wrapped `(coalton:coalton ,expr)))
                                 (eval wrapped)))
                         (error (e)
                           ;; If Coalton fails, try direct evaluation
                           (format *error-output* "Coalton eval failed, trying direct: ~A~%" e)
                           (eval expr)))))
          (format t "~A~%" result)
          0))  ; Success exit code
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      1)))  ; Error exit code

;;; ===========================================================================
;;; STAGE 4: Ultra-fast Main Entry Point
;;; ===========================================================================

(defun main ()
  "Main entry point - minimal overhead"
  ;; Don't load ANYTHING until we know what's needed
  (let ((args (cdr sb-ext:*posix-argv*)))  ; Skip program name
    (when (null args)
      (format t "Usage: smt [eval|repl|run|version] ...~%")
      (sb-ext:exit :code 1))
    
    (let ((command (first args))
          (rest-args (rest args)))
      (cond
        ;; VERSION - Don't load anything!
        ((string= command "version")
         (format t "Smelter v0.1.0 (fast edition)~%")
         (format t "Ultra-fast startup with lazy loading~%")
         (sb-ext:exit :code 0))
        
        ;; EVAL - Fast path for simple evaluation
        ((string= command "eval")
         (when rest-args
           (let ((exit-code (eval-minimal (first rest-args))))
             (sb-ext:exit :code exit-code))))
        
        ;; REPL - Load everything for interactive use
        ((string= command "repl")
         (ensure-stdlib)  ; Load standard library for REPL
         (ensure-adapters) ; Load adapters for full functionality
         ;; Load full CLI if available
         (handler-case
             (let* ((cwd (or *default-pathname-defaults* (truename ".")))
                    (fasl (merge-pathnames "src/cli.fasl" cwd))
                    (source (merge-pathnames "src/cli.lisp" cwd)))
               (cond
                 ((probe-file fasl) (load fasl :verbose nil :print nil))
                 ((probe-file source) (load source :verbose nil :print nil)))
               ;; Start REPL using the full Smelter system
               (if (find-package :smelter)
                   (funcall (find-symbol "START-REPL" :smelter))
                   ;; Fallback simple REPL
                   (progn
                     (format t "Smelter Fast REPL (fallback mode)~%")
                     (format t "Type expressions to evaluate, :quit to exit~%")
                     (loop
                       (format t "smt> ")
                       (finish-output)
                       (let ((input (read-line)))
                         (cond
                           ((string= input ":quit") (return))
                           ((string= input "") nil)  ; Empty line
                           (t (eval-minimal input))))))))
           (error (e)
             (format *error-output* "Could not start REPL: ~A~%" e)
             (sb-ext:exit :code 1))))
        
        ;; RUN - Load everything for script execution
        ((string= command "run")
         (ensure-stdlib)
         (ensure-adapters)
         (when rest-args
           (let ((script-file (first rest-args)))
             (handler-case
                 (load script-file)
               (error (e)
                 (format *error-output* "Error running script ~A: ~A~%" script-file e)
                 (sb-ext:exit :code 1))))))
        
        ;; Unknown command
        (t
         (format *error-output* "Unknown command: ~A~%" command)
         (format *error-output* "Use: smt version|eval|repl|run~%")
         (sb-ext:exit :code 1))))))

;;; ===========================================================================
;;; STAGE 5: Build Configuration
;;; ===========================================================================

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
  
  ;; Minimal GC settings
  (when (fboundp 'sb-ext:gc)
    (sb-ext:gc :full t)))

(defun build-fast-image ()
  "Build the optimized Smelter image"
  (format t "Building fast Smelter image...~%")
  
  ;; Step 1: Configure SBCL for speed
  (configure-sbcl-for-speed)
  
  ;; Step 2: Load ONLY the minimal core (this file)
  ;; Do NOT load Coalton, Quicklisp, or anything else at build time!
  (format t "Core configuration complete.~%")
  
  ;; Step 3: Save the image with maximum compression
  (format t "Saving optimized image...~%")
  (sb-ext:save-lisp-and-die 
   "smt-fast"
   :toplevel #'smelter-fast:main
   :executable t
   :compression t  ; Enable core compression
   :save-runtime-options t))

;;; ===========================================================================
;;; STAGE 6: Alternative Minimal Arithmetic-Only Build
;;; For testing absolute minimum startup time
;;; ===========================================================================

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

(defun build-minimal-test-image ()
  "Build ultra-minimal image for testing baseline performance"
  (configure-sbcl-for-speed)
  (sb-ext:save-lisp-and-die
   "smt-minimal"
   :toplevel #'arithmetic-only-main
   :executable t
   :compression t))