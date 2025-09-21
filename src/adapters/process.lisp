(defpackage #:smelter/adapters/process
  (:use #:coalton #:coalton-prelude)
  (:export
   #:ProcessResult
   #:ProcessError #:CommandNotFound #:ExecutionError
   #:ProcessConfig
   #:run-process
   #:run-process-with-input
   #:shell
   #:spawn-process
   #:capture-command
   #:command-exists?
   #:run-with-sudo
   #:process-exit-code
   #:process-stdout
   #:process-stderr
   #:make-process-config))

(in-package #:smelter/adapters/process)

(coalton-toplevel
  (define-type ProcessResult
    (ProcessResult Integer String String))  ; exit-code, stdout, stderr

  (define-type ProcessError
    (CommandNotFound String)
    (ExecutionError String))

  (define-type ProcessConfig
    (ProcessConfig (Optional String) (Optional (List String)) (Optional Integer)))  ; working-dir, env, timeout

  ;; Accessors for ProcessResult
  (declare process-exit-code (ProcessResult -> Integer))
  (define (process-exit-code result)
    "Get exit code from ProcessResult"
    (match result
      ((ProcessResult code _ _) code)))

  (declare process-stdout (ProcessResult -> String))
  (define (process-stdout result)
    "Get stdout from ProcessResult"
    (match result
      ((ProcessResult _ stdout _) stdout)))

  (declare process-stderr (ProcessResult -> String))
  (define (process-stderr result)
    "Get stderr from ProcessResult"
    (match result
      ((ProcessResult _ _ stderr) stderr)))

  ;; Constructor for ProcessConfig
  (declare make-process-config ((Optional String) -> (Optional (List String)) -> (Optional Integer) -> ProcessConfig))
  (define (make-process-config working-dir env timeout)
    "Create ProcessConfig"
    (ProcessConfig working-dir env timeout))

  ;; Core process execution using sb-ext:run-program
  (declare run-process (String -> (Result ProcessError ProcessResult)))
  (define (run-process command)
    "Execute command and capture output"
    (lisp (Result ProcessError ProcessResult) (command)
      (cl:handler-case
          (cl:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program command 
                                :output :string 
                                :error-output :string
                                :ignore-error-status cl:t)
            (Ok (ProcessResult exit-code 
                               (cl:if stdout stdout "")
                               (cl:if stderr stderr ""))))
        (cl:error (e)
          (cl:cond
            ((cl:search "not found" (cl:format cl:nil "~A" e))
             (Err (CommandNotFound command)))
            (cl:t (Err (ExecutionError (cl:format cl:nil "Process error: ~A" e)))))))))

  ;; Process execution with input
  (declare run-process-with-input (String -> String -> (Result ProcessError ProcessResult)))
  (define (run-process-with-input command input)
    "Execute command with stdin input"
    (lisp (Result ProcessError ProcessResult) (command input)
      (cl:handler-case
          (cl:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program command 
                                :input (cl:make-string-input-stream input)
                                :output :string 
                                :error-output :string
                                :ignore-error-status cl:t)
            (Ok (ProcessResult exit-code 
                               (cl:if stdout stdout "")
                               (cl:if stderr stderr ""))))
        (cl:error (e)
          (cl:cond
            ((cl:search "not found" (cl:format cl:nil "~A" e))
             (Err (CommandNotFound command)))
            (cl:t (Err (ExecutionError (cl:format cl:nil "Process error: ~A" e)))))))))

  ;; Shell command execution
  (declare shell (String -> (Result ProcessError ProcessResult)))
  (define (shell command)
    "Execute command in shell"
    (lisp (Result ProcessError ProcessResult) (command)
      (cl:let ((shell-cmd #+unix (cl:list "/bin/sh" "-c" command)
                          #+windows (cl:list "cmd" "/c" command)
                          #-(or unix windows) (cl:error "Unsupported platform")))
        (cl:handler-case
            (cl:multiple-value-bind (stdout stderr exit-code)
                (uiop:run-program shell-cmd
                                  :output :string 
                                  :error-output :string
                                  :ignore-error-status cl:t)
              (Ok (ProcessResult exit-code 
                                 (cl:if stdout stdout "")
                                 (cl:if stderr stderr ""))))
          (cl:error (e)
            (Err (ExecutionError (cl:format cl:nil "Shell error: ~A" e))))))))

  ;; Async process spawning (simplified - returns immediately)
  (declare spawn-process (String -> (Result ProcessError ProcessResult)))
  (define (spawn-process command)
    "Spawn process asynchronously (simplified version)"
    ;; For now, just run synchronously
    (run-process command))

  ;; Simple command output capture
  (declare capture-command (String -> (Result ProcessError String)))
  (define (capture-command command)
    "Execute command and return stdout only"
    (match (run-process command)
      ((Ok result) (Ok (process-stdout result)))
      ((Err e) (Err e))))

  ;; Check if command exists
  (declare command-exists? (String -> Boolean))
  (define (command-exists? command)
    "Check if command exists in PATH"
    (lisp Boolean (command)
      (cl:handler-case
          (cl:progn
            #+unix (uiop:run-program (cl:list "which" command) :ignore-error-status cl:t)
            #+windows (uiop:run-program (cl:list "where" command) :ignore-error-status cl:t)
            #-(or unix windows) (cl:error "Unsupported platform")
            cl:t)
        (cl:error () cl:nil))))

  ;; Run command with sudo (Unix only)
  (declare run-with-sudo (String -> (Result ProcessError ProcessResult)))
  (define (run-with-sudo command)
    "Execute command with sudo"
    (lisp (Result ProcessError ProcessResult) (command)
      #+unix 
      (cl:handler-case
          (cl:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program (cl:list "sudo" "sh" "-c" command)
                                :output :string 
                                :error-output :string
                                :ignore-error-status cl:t)
            (Ok (ProcessResult exit-code 
                               (cl:if stdout stdout "")
                               (cl:if stderr stderr ""))))
        (cl:error (e)
          (Err (ExecutionError (cl:format cl:nil "Sudo error: ~A" e)))))
      #-unix (Err (ExecutionError "sudo not supported on this platform")))))