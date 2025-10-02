(defpackage :smelter/adapters/process
  (:documentation "
Smelter Process/Shell Adapter - Simplified String-Based Implementation

A type-safe process execution adapter that avoids Coalton compatibility issues
by using string-based commands instead of list-based arguments.

Key Features:
- Type-safe command execution with Result types for error handling
- Shell command support with input/output capture
- Environment variable manipulation
- Command piping and timeout support
- Cross-platform compatibility via UIOP

Design Philosophy:
- All fallible operations return (Result T ProcessError) for error handling
- Commands are represented as strings to avoid Coalton list conversion issues
- Simple types that don't conflict with Coalton's namespace system
- Direct FFI via lisp blocks using proven patterns from FS/HTTP adapters

Usage:
  (use-package :smelter/adapters/process)

  (match (run \"ls -la\")
    ((Ok (ProcessResult stdout stderr exit-code))
     (println stdout))
    ((Err (ProcessFailed msg)) (handle-error msg)))
")
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:lisp #:common-lisp))
  (:export
   ;; Types
   #:ProcessError #:ProcessFailed #:ProcessTimeout
   #:ProcessResult

   ;; Core execution
   #:run #:shell #:shell-with-input
   #:run-with-timeout

   ;; Environment
   #:get-env #:set-env

   ;; Utilities
   #:which #:pipe-commands))

(in-package :smelter/adapters/process)

(coalton-toplevel
  ;; Simplified error type - message with optional timeout variant
  (define-type ProcessError
    (ProcessFailed String)
    (ProcessTimeout String))

  ;; Simple result type - stdout, stderr, exit code
  (define-type ProcessResult
    (ProcessResult String String Integer))

  ;; Core execution - takes command as STRING
  (declare run (String -> (Result ProcessError ProcessResult)))
  (define (run command)
    "Execute a command string and capture output."
    (lisp (Result ProcessError ProcessResult) (command)
      (lisp:handler-case
        (lisp:multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program command
                            :output :string
                            :error-output :string
                            :ignore-error-status lisp:t)
          (Ok (ProcessResult
               (lisp:or stdout "")
               (lisp:or stderr "")
               (lisp:or exit-code 0))))
        (lisp:error (e)
          (Err (ProcessFailed (lisp:format lisp:nil "~A" e)))))))

  ;; Shell execution - wraps in sh -c for shell features
  (declare shell (String -> (Result ProcessError ProcessResult)))
  (define (shell command)
    "Execute command through shell (enables pipes, variables, etc)."
    (run (lisp String (command)
           (lisp:format lisp:nil "sh -c '~A'"
                    ;; Escape single quotes in the command
                    (lisp:substitute-if-not #\'
                                         (lisp:lambda (c) (lisp:char/= c #\'))
                                         command)))))

  ;; Shell with input
  (declare shell-with-input (String -> String -> (Result ProcessError ProcessResult)))
  (define (shell-with-input command input)
    "Execute shell command with stdin input."
    (lisp (Result ProcessError ProcessResult) (command input)
      (lisp:handler-case
        (lisp:with-input-from-string (input-stream input)
          (lisp:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program (lisp:format lisp:nil "sh -c '~A'" command)
                              :input input-stream
                              :output :string
                              :error-output :string
                              :ignore-error-status lisp:t)
            (Ok (ProcessResult
                 (lisp:or stdout "")
                 (lisp:or stderr "")
                 (lisp:or exit-code 0)))))
        (lisp:error (e)
          (Err (ProcessFailed (lisp:format lisp:nil "~A" e)))))))

  ;; Environment variable operations
  (declare get-env (String -> (Optional String)))
  (define (get-env name)
    "Get value of environment variable."
    (lisp (Optional String) (name)
      (lisp:let ((value (uiop:getenv name)))
        (lisp:if value
               (Some value)
               None))))

  (declare set-env (String -> String -> Unit))
  (define (set-env name value)
    "Set environment variable for current process."
    (lisp Unit (name value)
      (lisp:progn
        (lisp:setf (uiop:getenv name) value)
        Unit)))

  ;; Which - find executable in PATH
  (declare which (String -> (Optional String)))
  (define (which program)
    "Find full path of executable in PATH."
    (lisp (Optional String) (program)
      (lisp:handler-case
        (lisp:let ((cmd #+unix (lisp:format lisp:nil "which ~A" program)
                        #+windows (lisp:format lisp:nil "where ~A" program)
                        #-(or unix windows) (lisp:error "Unsupported platform")))
          (lisp:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program cmd
                              :output :string
                              :error-output :string
                              :ignore-error-status lisp:t)
            (lisp:if (lisp:zerop exit-code)
                     (Some (lisp:string-trim '(#\Newline #\Return #\Space) stdout))
                     None)))
        (lisp:error () None))))

  ;; Simple pipe - combines two commands
  (declare pipe-commands (String -> String -> (Result ProcessError ProcessResult)))
  (define (pipe-commands cmd1 cmd2)
    "Pipe output of cmd1 to cmd2."
    (match (run cmd1)
      ((Ok (ProcessResult stdout1 _ _))
       (shell-with-input cmd2 stdout1))
      ((Err e) (Err e))))

  ;; Timeout execution - uses timeout command on Unix
  (declare run-with-timeout (Integer -> String -> (Result ProcessError ProcessResult)))
  (define (run-with-timeout seconds command)
    "Execute command with timeout in seconds (Unix only)."
    (run (lisp String (seconds command)
           #+unix (lisp:format lisp:nil "timeout ~D ~A" seconds command)
           #-unix command))))  ; Fallback to no timeout on non-Unix
