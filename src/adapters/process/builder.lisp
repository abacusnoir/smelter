(defpackage :smelter/adapters/process/builder
  (:documentation "
Process Adapter Command Builder Module

Provides safe command construction utilities for building shell commands
from Coalton lists while preventing shell injection attacks.

Key Features:
- Automatic shell escaping for arguments
- List-to-string command construction
- Type-safe argument handling

Usage:
  (use-package :smelter/adapters/process/builder)

  ;; Build safe command from list
  (run-args \"grep\" (list \"-r\" \"search term\" \"/path\"))

  ;; Escape individual arguments
  (escape-shell-arg \"argument with spaces\")
")
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:lisp #:common-lisp)
                    (#:proc #:smelter/adapters/process))
  (:export
   #:escape-shell-arg
   #:build-command
   #:run-args))

(in-package :smelter/adapters/process/builder)

(coalton-toplevel
  ;; Escape shell argument by wrapping in single quotes
  (declare escape-shell-arg (String -> String))
  (define (escape-shell-arg arg)
    "Escape special characters in shell argument by wrapping in single quotes.
     Single quotes within the argument are properly escaped."
    (lisp String (arg)
      ;; Wrap in single quotes and escape any embedded single quotes
      ;; Strategy: Replace ' with '\''  (end quote, escaped quote, start quote)
      (lisp:let ((escaped (lisp:with-output-to-string (stream)
                           (lisp:loop for char across arg do
                             (lisp:if (lisp:char= char #\')
                                 ;; Replace ' with '\''
                                 (lisp:format stream "'\\''")
                                 (lisp:write-char char stream))))))
        ;; Wrap the whole thing in single quotes
        (lisp:format lisp:nil "'~A'" escaped))))

  ;; Build command string from command and list of arguments
  (declare build-command (String -> (List String) -> String))
  (define (build-command cmd args)
    "Build command string from command and arguments with proper escaping.
     Each argument is escaped to prevent shell injection."
    ;; Coalton fold: (fn (acc element) ...) initial-value list
    ;; We want: cmd arg1 arg2 arg3
    (fold (fn (acc escaped-arg)
            ;; Concatenate: acc + space + escaped-arg
            (lisp String (acc escaped-arg)
              (lisp:format lisp:nil "~A ~A" acc escaped-arg)))
          cmd
          (map escape-shell-arg args)))

  ;; Run command with list of arguments (safe wrapper around process:run)
  (declare run-args (String -> (List String) -> (coalton-library/classes:Result proc:ProcessError proc:ProcessResult)))
  (define (run-args cmd args)
    "Execute command with list of arguments (safe alternative to string construction).
     Arguments are automatically escaped to prevent shell injection.

     Example:
       (run-args \"grep\" (list \"-r\" \"pattern with spaces\" \"/path\"))
       ;; Executes: grep -r 'pattern with spaces' '/path'
     "
    (proc:run (build-command cmd args))))
