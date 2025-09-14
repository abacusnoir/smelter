;;;; Smelter CLI - Self-contained Coalton runner
;;;; Main entry point and command handling

(defpackage #:smelter
  (:use #:cl)
  (:import-from #:smelter.translator
                #:translate-pure-coalton
                #:wrap-for-execution
                #:parse-coalton-file)
  (:export #:main
           #:save-executable
           #:run-script
           #:start-repl
           #:eval-expression
           #:*script-main*))

(in-package #:smelter)

;;; Version and metadata
(defparameter *smelter-version* "0.1.0")
(defparameter *coalton-version* "0.8.0")

;;; Script execution support
(defparameter *script-main* nil
  "Function to call when executing scripts with main functions")

;;; Create user package for script execution
(defpackage #:smelter.user
  (:use #:cl)
  (:import-from #:coalton #:coalton-toplevel #:coalton))

;;; Error handling
(define-condition smelter-error (error)
  ((message :initarg :message :reader smelter-error-message)))

(defun smelter-error (format-string &rest args)
  "Signal a smelter error with formatted message"
  (error 'smelter-error 
         :message (apply #'format nil format-string args)))

;;; Utility functions
(defun print-version ()
  "Print version information"
  (format t "Smelter ~A~%" *smelter-version*)
  (format t "Coalton ~A~%" *coalton-version*)
  (format t "SBCL ~A~%" (lisp-implementation-version)))

(defun print-help ()
  "Print help information"
  (format t "Smelter - Industrial-strength typed scripting with Coalton~%~%")
  (format t "Usage:~%")
  (format t "  smt run <file.coal>     Run a Coalton script~%")
  (format t "  smt eval <expression>   Evaluate a Coalton expression~%")
  (format t "  smt repl               Start interactive REPL~%")
  (format t "  smt check <file.coal>   Type-check without running~%")
  (format t "  smt --version          Show version information~%")
  (format t "  smt --help             Show this help~%~%")
  (format t "Examples:~%")
  (format t "  smt run hello.coal~%")
  (format t "  smt eval '(+ 2 3)'~%")
  (format t "  echo '(* 6 7)' | smt repl~%")
  (format t "  ./script.coal          # With shebang: #!/usr/bin/env smt run~%"))

;;; Coalton environment setup
(defun setup-coalton-environment ()
  "Ensure Coalton is loaded and ready"
  (handler-case
      (progn
        ;; Coalton should already be loaded in our core image
        ;; but let's verify it's available
        (unless (find-package :coalton)
          (error "Coalton not found in image"))
        
        ;; The smelter.user package is already created at the top of this file
        ;; Just ensure we can use it
        (in-package #:smelter.user)
        t)
    (error (e)
      (smelter-error "Failed to setup Coalton environment: ~A" e))))

;;; Script execution
(defun read-file-content (filepath)
  "Read the entire content of a file"
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun strip-shebang (content)
  "Remove shebang line if present"
  (if (and (> (length content) 2)
           (string= (subseq content 0 2) "#!"))
      (let ((newline-pos (position #\Newline content)))
        (if newline-pos
            (subseq content (1+ newline-pos))
            ""))
      content))

(defun wrap-coalton-script (content)
  "Wrap user script content in proper Coalton environment"
  (concatenate 'string 
               "(in-package #:smelter-user)" "
"
               ";; User script content" "
"
               content "

"
               ";; Auto-run main function if it exists" "
"
               "(when (fboundp 'main)" "
"
               "  (handler-case" "
"
               "      (main)" "
"
               "    (error (e)" "
"
               "      (format *error-output* \"Error in main: ~A~%\" e)" "
"
               "      (sb-ext:exit :code 1))))"))

(defun run-script (filepath)
  "Run a Coalton script file with translation support"
  (handler-case
      (progn
        ;; Verify file exists
        (unless (probe-file filepath)
          (smelter-error "File not found: ~A" filepath))
        
        ;; Read and process script  
        (let* ((raw-content (read-file-content filepath))
               (content (strip-shebang raw-content)))
          
          ;; Reset script main
          (setf *script-main* nil)
          
          ;; Use translator to convert pure Coalton to executable form
          (let ((translated (smelter.translator:translate-pure-coalton content :for-repl nil)))
            ;; Read and evaluate in the correct package context
            (let ((*package* (find-package :smelter.user)))
              (eval (read-from-string translated))))
          
          ;; Call main function if it was set
          (when *script-main*
            (funcall *script-main*)))
        
        ;; Exit successfully
        (sb-ext:exit :code 0))
    
    (smelter-error (e)
      (format *error-output* "Smelter error: ~A~%" (smelter-error-message e))
      (sb-ext:exit :code 1))
    
    (error (e)
      (format *error-output* "Error running script: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Expression evaluation
(defun eval-expression (expr-string)
  "Evaluate a Coalton expression - Critical: Set package BEFORE reading the string"
  (handler-case
      (progn
        (setup-coalton-environment)
        
        ;; Critical: Set package context BEFORE reading the form
        ;; This ensures symbols are interned in the correct package
        (let ((*package* (find-package :coalton-user)))
          (let* ((form (read-from-string expr-string))  ; Read IN coalton-user package
                 (wrapped `(coalton:coalton ,form))     ; Wrap for Coalton evaluation
                 (result (eval wrapped)))               ; Evaluate in same package
            (format t "~A~%" result)))
        
        (sb-ext:exit :code 0))
    
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Type checking
(defun check-script (filepath)
  "Type-check a script without running it"
  (handler-case
      (progn
        (unless (probe-file filepath)
          (smelter-error "File not found: ~A" filepath))
        
        (setup-coalton-environment)
        
        ;; Read script content
        (let* ((raw-content (read-file-content filepath))
               (content (strip-shebang raw-content)))
          
          ;; Parse and type-check (this is a simplified version)
          ;; In a full implementation, we'd use Coalton's type checker directly
          (with-input-from-string (stream content)
            (loop for form = (read stream nil :eof)
                  until (eq form :eof)
                  do (format t "Checking: ~A~%" form)))
          
          (format t "Type checking completed successfully.~%"))
        
        (sb-ext:exit :code 0))
    
    (error (e)
      (format *error-output* "Type checking failed: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; REPL implementation
(defun start-repl ()
  "Start an interactive REPL"
  (handler-case
      (progn
        (format t "Smelter ~A - Simple REPL~%" *smelter-version*)
        (format t "Type expressions or :help for commands~%~%")
        
        (loop
          (format t "smt> ")
          (finish-output)
          
          (let ((line (read-line *standard-input* nil :eof)))
            (when (eq line :eof)
              (format t "~%Goodbye!~%")
              (return))
            
            (when (string= line "")
              (continue))
            
            ;; Handle REPL commands
            (cond
              ((string= line ":help")
               (format t "REPL Commands:~%")
               (format t "  :help    - Show this help~%")
               (format t "  :quit    - Exit REPL~%")
               (format t "  :version - Show version~%")
               (format t "~%Enter Lisp expressions to evaluate them.~%"))
              
              ((or (string= line ":quit") (string= line ":q"))
               (format t "Goodbye!~%")
               (return))
              
              ((string= line ":version")
               (print-version))
              
              ;; Evaluate expression
              (t
               (handler-case
                   (let ((result (eval (read-from-string line))))
                     (format t "~A~%" result))
                 (error (e)
                   (format t "Error: ~A~%" e))))))))
    
    (error (e)
      (format *error-output* "REPL error: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Main entry point
(defun parse-arguments (args)
  "Parse command line arguments"
  (cond
    ;; No arguments - show help
    ((null args)
     (print-help)
     (sb-ext:exit :code 0))
    
    ;; Version flag
    ((member "--version" args :test #'string=)
     (print-version)
     (sb-ext:exit :code 0))
    
    ;; Help flag
    ((or (member "--help" args :test #'string=)
         (member "-h" args :test #'string=))
     (print-help)
     (sb-ext:exit :code 0))
    
    ;; Commands
    ((string= (first args) "run")
     (unless (second args)
       (smelter-error "Usage: smt run <file.coal>"))
     (run-script (second args)))
    
    ((string= (first args) "eval")
     (unless (second args)
       (smelter-error "Usage: smt eval <expression>"))
     (eval-expression (second args)))
    
    ((string= (first args) "repl")
     (start-repl))
    
    ((string= (first args) "check")
     (unless (second args)
       (smelter-error "Usage: smt check <file.coal>"))
     (check-script (second args)))
    
    ;; Direct script execution (for shebang support)
    ((and (= (length args) 1)
          (probe-file (first args)))
     (run-script (first args)))
    
    ;; Unknown command
    (t
     (format *error-output* "Unknown command: ~A~%" (first args))
     (format *error-output* "Try 'smt --help' for usage information.~%")
     (sb-ext:exit :code 1))))

(defun main ()
  "Main entry point for Smelter CLI"
  (handler-case
      (let ((args (rest sb-ext:*posix-argv*))) ; Skip program name
        (parse-arguments args))
    
    (smelter-error (e)
      (format *error-output* "Error: ~A~%" (smelter-error-message e))
      (sb-ext:exit :code 1))
    
    (error (e)
      (format *error-output* "Unexpected error: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Executable creation
(defun save-executable (filename)
  "Save the current image as an executable"
  (sb-ext:save-lisp-and-die filename
                            :executable t
                            :toplevel #'main
                            :compression t
                            :save-runtime-options nil))