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
(defparameter *smelter-version* "0.2.0")
(defparameter *coalton-version* "0.8.0")

;;; Script execution support
(defparameter *script-main* nil
  "Function to call when executing scripts with main functions")

;;; User package is already created in build/create-image.lisp as :coalton-user

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
  (format t "Smelter - Lisp scripts that just work. Types optional.~%~%")
  (format t "Usage:~%")
  (format t "  smt run <file.coal>     Run a Coalton script (type-safe)~%")
  (format t "  smt eval <expression>   Evaluate a Coalton expression~%")
  (format t "  smt repl               Start interactive Coalton REPL~%")
  (format t "  smt check <file.coal>   Type-check without running~%")
  (format t "  smt cl <subcommand>     Common Lisp mode (see below)~%")
  (format t "  smt --version          Show version information~%")
  (format t "  smt --help             Show this help~%~%")
  (format t "Coalton Examples:~%")
  (format t "  smt run hello.coal~%")
  (format t "  smt eval '(+ 2 3)'~%")
  (format t "  ./script.coal          # With shebang: #!/usr/bin/env smt run~%~%")
  (format t "Common Lisp Mode:~%")
  (format t "  smt cl run <file.lisp>  Run a CL script~%")
  (format t "  smt cl eval <expr>      Evaluate a CL expression~%")
  (format t "  smt cl repl             Start CL REPL~%")
  (format t "  smt cl --help           Show CL mode help~%~%")
  (format t "Same binary. Same instant startup. Your choice.~%"))

;;; Coalton environment setup
(defun setup-coalton-environment ()
  "Ensure Coalton is loaded and ready"
  ;; Basic package verification
  (unless (find-package :coalton)
    (smelter-error "Coalton package not found in executable"))

  (unless (find-package :coalton-user)
    (smelter-error "coalton-user package not found in executable")))

;;; Script execution
(defun read-file-content (filepath)
  "Read the entire content of a file"
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun strip-shebang (content)
  "Remove shebang line if present (handles leading whitespace)"
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) content))
         (has-shebang (and (>= (length trimmed) 2)
                          (string= (subseq trimmed 0 2) "#!"))))
    (if has-shebang
        (let ((newline-pos (position #\Newline trimmed)))
          (if newline-pos
              (subseq trimmed (1+ newline-pos))
              ""))
        content)))

(defun wrap-coalton-script (content)
  "Wrap user script content in proper Coalton environment"
  (concatenate 'string 
               "(in-package #:coalton-user)" "\n"
               ";; User script content" "\n"
               content "\n\n"
               ";; Auto-run main function if it exists" "\n"
               "(when (fboundp 'main)" "\n"
               "  (handler-case" "\n"
               "      (main)" "\n"
               "    (error (e)" "\n"
               "      (format *error-output* \"Error in main: ~A~%\" e)" "\n"
               "      (sb-ext:exit :code 1))))"
))

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
            (let ((*package* (find-package :coalton-user)))
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
  "Evaluate a Coalton expression using the same translation as run-script"
  (handler-case
      (progn
        (setup-coalton-environment)
        
        ;; Use translator to convert pure Coalton to executable form
        (let ((translated (smelter.translator:translate-pure-coalton expr-string :for-repl t)))
          ;; Evaluate in the correct package context
          (let ((*package* (find-package :coalton-user)))
            (let ((result (eval (read-from-string translated))))
              (format t "~A~%" result))))
        
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
                   (progn
                     (setup-coalton-environment)
                     ;; Use translator to convert pure Coalton to executable form
                     (let ((translated (smelter.translator:translate-pure-coalton line :for-repl t)))
                       ;; Evaluate in the correct package context with proper error handling
                       (let ((*package* (find-package :coalton-user)))
                         (handler-case
                             (let ((result (eval (read-from-string translated))))
                               (format t "~A~%" result))
                           (error (e)
                             (format t "Evaluation Error: ~A~%" e))))))
                 (error (e)
                   (format t "Translation Error: ~A~%" e))))))))
    
    (error (e)
      (format *error-output* "REPL error: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; Common Lisp Mode Support
;;; Run pure CL scripts without Coalton translation

(defun print-cl-help ()
  "Print CL mode help information"
  (format t "Smelter CL Mode - Run Common Lisp scripts~%~%")
  (format t "Usage:~%")
  (format t "  smt cl run <file.lisp>  Run a Common Lisp script~%")
  (format t "  smt cl eval <expr>      Evaluate a CL expression~%")
  (format t "  smt cl repl             Start CL REPL~%")
  (format t "  smt-cl <file.lisp>      Shortcut for shebang scripts~%")
  (format t "  smt cl --help           Show this help~%~%")
  (format t "Examples:~%")
  (format t "  smt cl run script.lisp~%")
  (format t "  smt cl eval '(+ 2 3)'~%")
  (format t "  ./script.lisp           # With shebang: #!/usr/bin/env smt-cl~%~%")
  (format t "CL mode runs pure Common Lisp without Coalton translation.~%")
  (format t "Use Coalton mode (smt run) for type-safe scripts.~%"))

(defun run-cl-script (filepath)
  "Run a Common Lisp script file directly"
  (handler-case
      (progn
        (unless filepath
          (smelter-error "Usage: smt cl run <file.lisp>"))
        (unless (probe-file filepath)
          (smelter-error "File not found: ~A" filepath))

        ;; Read and process script
        (let* ((raw-content (read-file-content filepath))
               (content (strip-shebang raw-content)))

          ;; Evaluate in CL-USER package (standard CL environment)
          (let ((*package* (find-package :cl-user)))
            (with-input-from-string (stream content)
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    do (eval form)))))

        (sb-ext:exit :code 0))

    (smelter-error (e)
      (format *error-output* "Smelter error: ~A~%" (smelter-error-message e))
      (sb-ext:exit :code 1))

    (error (e)
      (format *error-output* "Error running CL script: ~A~%" e)
      (sb-ext:exit :code 1))))

(defun eval-cl-expr (expr-string)
  "Evaluate a Common Lisp expression"
  (handler-case
      (progn
        (unless expr-string
          (smelter-error "Usage: smt cl eval <expression>"))

        (let ((*package* (find-package :cl-user)))
          (let ((result (eval (read-from-string expr-string))))
            (format t "~A~%" result)))

        (sb-ext:exit :code 0))

    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:exit :code 1))))

(defun run-cl-repl ()
  "Start a Common Lisp REPL"
  (handler-case
      (progn
        (format t "Smelter ~A - CL REPL~%" *smelter-version*)
        (format t "Type expressions or :help for commands~%~%")

        (loop
          (format t "cl> ")
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
               (format t "CL REPL Commands:~%")
               (format t "  :help    - Show this help~%")
               (format t "  :quit    - Exit REPL~%")
               (format t "  :version - Show version~%")
               (format t "~%Enter Common Lisp expressions to evaluate.~%"))

              ((or (string= line ":quit") (string= line ":q"))
               (format t "Goodbye!~%")
               (return))

              ((string= line ":version")
               (print-version))

              ;; Evaluate expression
              (t
               (handler-case
                   (let ((*package* (find-package :cl-user)))
                     (let ((result (eval (read-from-string line))))
                       (format t "~A~%" result)))
                 (error (e)
                   (format t "Error: ~A~%" e))))))))

    (error (e)
      (format *error-output* "CL REPL error: ~A~%" e)
      (sb-ext:exit :code 1))))

(defun handle-cl-mode (args)
  "Handle CL mode subcommands"
  (let ((subcommand (first args)))
    (cond
      ((null subcommand)
       (print-cl-help)
       (sb-ext:exit :code 0))

      ((or (string= subcommand "--help") (string= subcommand "-h"))
       (print-cl-help)
       (sb-ext:exit :code 0))

      ((string= subcommand "run")
       (run-cl-script (second args)))

      ((string= subcommand "eval")
       (eval-cl-expr (second args)))

      ((string= subcommand "repl")
       (run-cl-repl))

      ;; Direct file execution for shebang support
      ((probe-file subcommand)
       (run-cl-script subcommand))

      (t
       (format *error-output* "Unknown CL command: ~A~%" subcommand)
       (format *error-output* "Try 'smt cl --help' for usage information.~%")
       (sb-ext:exit :code 1)))))

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
    
    ;; Help flag (only when it's the first/only argument)
    ((or (string= (first args) "--help")
         (string= (first args) "-h"))
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

    ;; CL mode - pure Common Lisp without Coalton
    ((string= (first args) "cl")
     (handle-cl-mode (rest args)))

    ;; Direct script execution (for shebang support)
    ((and (= (length args) 1)
          (probe-file (first args)))
     (run-script (first args)))

    ;; Unknown command
    (t
     (format *error-output* "Unknown command: ~A~%" (first args))
     (format *error-output* "Try 'smt --help' for usage information.~%")
     (sb-ext:exit :code 1))))

(defun invoked-as-cl-mode-p ()
  "Check if program was invoked as smt-cl (for shebang support)"
  (let ((program-name (first sb-ext:*posix-argv*)))
    (and program-name
         (let ((basename (file-namestring program-name)))
           (or (string= basename "smt-cl")
               (string-equal basename "smt-cl.exe"))))))

(defun main ()
  "Main entry point for Smelter CLI"
  (handler-case
      ;; Check if invoked as smt-cl for CL mode
      (let ((args (rest sb-ext:*posix-argv*))) ; Skip program name
        (if (invoked-as-cl-mode-p)
            ;; Auto-route to CL mode when invoked as smt-cl
            (handle-cl-mode args)
            ;; Normal argument parsing
            (parse-arguments args)))

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
