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
    (ProcessResult Integer String String))

  (define-type ProcessError
    (CommandNotFound String)
    (ExecutionError String))

  (define-type ProcessConfig
    (ProcessConfig (Optional String) (Optional (List String)) (Optional Integer)))

  (declare handle-process-error (cl:condition -> ProcessError))
  (define (handle-process-error condition)
    "Convert Common Lisp process condition to ProcessError"
    (lisp ProcessError (condition)
      (cl:make-instance 'ExecutionError 
                        :string (cl:format cl:nil "Process execution error: ~A" condition))))

  (declare run-process (String -> (Result ProcessResult ProcessError)))
  (define (run-process command)
    "Execute command and capture output"
    (lisp (Result ProcessResult ProcessError) (command)
      (cl:handler-case
          (cl:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program command 
                                :output :string 
                                :error-output :string
                                :ignore-error-status t)
            (cl:values
             (cl:make-instance 'coalton:Ok
                               :ok (cl:make-instance 'ProcessResult
                                                     :integer exit-code
                                                     :string (cl:or stdout "")
                                                     :string (cl:or stderr "")))
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-process-error e))
           cl:t)))))

  (declare run-process-with-input (String -> String -> (Result ProcessResult ProcessError)))
  (define (run-process-with-input command input-str)
    "Execute command with stdin input"
    (lisp (Result ProcessResult ProcessError) (command input-str)
      (cl:handler-case
          (cl:multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program command
                                :input (cl:make-string-input-stream input-str)
                                :output :string
                                :error-output :string 
                                :ignore-error-status t)
            (cl:values
             (cl:make-instance 'coalton:Ok
                               :ok (cl:make-instance 'ProcessResult
                                                     :integer exit-code  
                                                     :string (cl:or stdout "")
                                                     :string (cl:or stderr "")))
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-process-error e))
           cl:t)))))

  (declare shell (String -> (Result ProcessResult ProcessError)))
  (define (shell command)
    "Run command in system shell"
    (let ((shell-command (get-shell-command command)))
      (run-process shell-command)))

  (declare get-shell-command (String -> String))
  (define (get-shell-command command)
    "Wrap command for shell execution"
    (lisp String (command)
      #+unix (cl:format cl:nil "/bin/sh -c '~A'" command)
      #+windows (cl:format cl:nil "cmd /c \"~A\"" command)
      #-(or unix windows) command))

  (declare spawn-process (String -> (Result Integer ProcessError)))
  (define (spawn-process command)
    "Start process asynchronously and return process ID"
    (lisp (Result Integer ProcessError) (command)
      (cl:handler-case
          (cl:let ((process (uiop:launch-program command)))
            (cl:values
             (cl:make-instance 'coalton:Ok 
                               :ok (uiop:process-info-pid process))
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-process-error e))
           cl:t)))))

  (declare capture-command (String -> (Result String ProcessError)))
  (define (capture-command command)
    "Simple stdout capture"
    (match (run-process command)
      ((Ok result) (Ok (process-stdout result)))
      ((Err e) (Err e))))

  (declare command-exists? (String -> Boolean))
  (define (command-exists? command)
    "Check if command exists in PATH"
    (lisp Boolean (command)
      (cl:handler-case
          (cl:progn
            #+unix (uiop:run-program (cl:format cl:nil "which ~A" command) 
                                     :ignore-error-status t)
            #+windows (uiop:run-program (cl:format cl:nil "where ~A" command)
                                        :ignore-error-status t)
            #-(or unix windows) (cl:error "Platform not supported")
            cl:t)
        (cl:condition (e) 
          (cl:declare (cl:ignore e))
          cl:nil))))

  (declare run-with-sudo (String -> (Result ProcessResult ProcessError)))
  (define (run-with-sudo command)
    "Run command with sudo (Unix only)"
    (lisp (Result ProcessResult ProcessError) (command)
      #+unix 
      (run-process (cl:format cl:nil "sudo ~A" command))
      #-unix
      (cl:values
       (cl:make-instance 'coalton:Err
                         :err (cl:make-instance 'ExecutionError
                                                :string "sudo not available on this platform"))
       cl:t)))

  (declare process-exit-code (ProcessResult -> Integer))
  (define (process-exit-code result)
    "Get exit code from process result"
    (match result
      ((ProcessResult code _ _) code)))

  (declare process-stdout (ProcessResult -> String))
  (define (process-stdout result)
    "Get stdout from process result"  
    (match result
      ((ProcessResult _ stdout _) stdout)))

  (declare process-stderr (ProcessResult -> String))
  (define (process-stderr result)
    "Get stderr from process result"
    (match result
      ((ProcessResult _ _ stderr) stderr)))

  (declare make-process-config ((Optional String) -> (Optional (List String)) -> (Optional Integer) -> ProcessConfig))
  (define (make-process-config working-dir env-vars timeout)
    "Create ProcessConfig with options"
    (ProcessConfig working-dir env-vars timeout))

  (declare run-process-with-config (String -> ProcessConfig -> (Result ProcessResult ProcessError)))
  (define (run-process-with-config command config)
    "Execute process with configuration options"
    (match config
      ((ProcessConfig working-dir env-vars timeout)
       (lisp (Result ProcessResult ProcessError) (command working-dir env-vars timeout)
         (cl:handler-case
             (cl:let ((old-dir (cl:when working-dir (uiop:getcwd))))
               (cl:when working-dir (uiop:chdir working-dir))
               (cl:unwind-protect
                    (cl:multiple-value-bind (stdout stderr exit-code)
                        (uiop:run-program command
                                          :output :string
                                          :error-output :string
                                          :ignore-error-status t)
                      (cl:values
                       (cl:make-instance 'coalton:Ok
                                         :ok (cl:make-instance 'ProcessResult
                                                               :integer exit-code
                                                               :string (cl:or stdout "")
                                                               :string (cl:or stderr "")))
                       cl:t))
                 (cl:when old-dir (uiop:chdir old-dir))))
           (cl:condition (e)
             (cl:values
              (cl:make-instance 'coalton:Err :err (handle-process-error e))
              cl:t))))))))