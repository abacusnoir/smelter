(defpackage :smelter/adapters/process/options
  (:documentation "
Process Adapter Execution Options Module

Provides execution configuration for process commands:
- Working directory control
- Environment variable management
- Stdin input

Simplified Phase 2 implementation focusing on core execution options.

NOTE: Timeout functionality deferred to post-launch.
      macOS lacks timeout command, requires complex shell workaround.

Usage:
  (use-package :smelter/adapters/process/options)

  ;; Run with custom working directory
  (let ((opts (make-options (Some \"/tmp\") None None)))
    (run-with-options \"pwd\" opts))

  ;; Run with environment variables
  (let ((opts (make-options None (Some (list (Tuple \"VAR\" \"value\"))) None)))
    (run-with-options \"echo $VAR\" opts))
")
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:lisp #:common-lisp)
                    (#:proc #:smelter/adapters/process))
  (:export
   ;; Types
   #:ExecOptions

   ;; Option construction
   #:make-options

   ;; Sync execution with options
   #:run-with-options
   #:shell-with-options))

(in-package :smelter/adapters/process/options)

(coalton-toplevel
  ;; Execution options type
  (define-type ExecOptions
    "Options for configuring process execution"
    (ExecOptions
      (Optional String)                                ;; working-directory
      (Optional (List (Tuple String String)))          ;; environment variables
      (Optional String)))                              ;; stdin-input

  ;; Constructor for ExecOptions
  (declare make-options ((Optional String) -> (Optional (List (Tuple String String))) -> (Optional String) -> ExecOptions))
  (define (make-options working-dir env stdin)
    "Create ExecOptions with specified values"
    (ExecOptions working-dir env stdin))

  ;; Run command with options (sync)
  (declare run-with-options (String -> ExecOptions -> (coalton-library/classes:Result proc:ProcessError proc:ProcessResult)))
  (define (run-with-options command opts)
    "Execute command with specified options (synchronous)"
    (match opts
      ((ExecOptions working-dir env stdin)
       ;; Build environment string for shell
       (let ((env-prefix
               (match env
                 ((Some env-list)
                  ;; Build env string from list of (Tuple key val)
                  (fold
                    (fn (acc pair)
                      (match pair
                        ((Tuple key val)
                         (<> acc (lisp String (key val)
                                   (lisp:format lisp:nil "export ~A='~A'; " key val))))))
                    ""
                    env-list))
                 (None ""))))
         ;; Build cd prefix if working-dir specified
         (let ((cd-prefix
                 (match working-dir
                   ((Some dir) (lisp String (dir) (lisp:format lisp:nil "cd '~A'; " dir)))
                   (None ""))))
           ;; Build final command
           (let ((final-command
                   (lisp String (env-prefix cd-prefix command)
                     (lisp:format lisp:nil "~A~A~A" env-prefix cd-prefix command))))
             ;; Execute with stdin if provided
             (match stdin
               ((Some input) (proc:shell-with-input final-command input))
               (None (proc:shell final-command)))))))))

  ;; Shell with options (sync)
  (declare shell-with-options (String -> ExecOptions -> (coalton-library/classes:Result proc:ProcessError proc:ProcessResult)))
  (define (shell-with-options command opts)
    "Execute shell command with specified options (synchronous).
     Command is automatically wrapped in shell context."
    (run-with-options command opts)))
