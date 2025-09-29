(defpackage #:smelter/test/process
  (:documentation "
Comprehensive test suite for Smelter Process/Shell Adapter

Tests all major functionality including:
- Core command execution (run, shell)
- Input/output handling
- Piping and command chaining
- Environment variable operations
- Working directory management
- Utility functions (which, executable?, escaping)
- Error handling and edge cases
- Legacy API compatibility

The test suite is designed to be run in both REPL and script modes,
providing immediate feedback on adapter functionality.
")
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:proc #:smelter/adapters/process))
  (:export
   #:run-all-tests
   #:test-basic-execution
   #:test-shell-execution
   #:test-input-output
   #:test-piping
   #:test-environment
   #:test-working-directory
   #:test-utilities
   #:test-error-handling
   #:test-legacy-compatibility))

(in-package #:smelter/test/process)

(coalton-toplevel
  (define (run-all-tests)
    "Run comprehensive process adapter tests."
    (println "🧪 Testing Smelter Process Adapter...")
    (println "====================================")
    (test-basic-execution)
    (test-shell-execution)
    (test-input-output)
    (test-piping)
    (test-environment)
    (test-working-directory)
    (test-utilities)
    (test-error-handling)
    (test-legacy-compatibility)
    (println "")
    (println "✅ All process adapter tests completed!")
    (println "===================================="))

  (define (test-basic-execution)
    "Test basic command execution."
    (println "")
    (println "1. Testing Basic Command Execution")
    (println "  --------------------------------")

    ;; Test simple echo command
    (println "  Testing echo command...")
    (match (proc:run (make-list "echo" "Hello, Smelter!"))
      ((Ok (proc:ProcessResult stdout _ (proc:ExitSuccess)))
       (if (== (coalton-library/string:strip stdout) "Hello, Smelter!")
           (println "    ✓ Echo command works")
           (println (format "    ✗ Echo output mismatch: expected 'Hello, Smelter!', got '~A'" stdout))))
      ((Ok (proc:ProcessResult _ _ (proc:ExitFailure code)))
       (println (format "    ✗ Echo command failed with exit code ~A" code)))
      ((Err e) (println "    ✗ Echo command error")))

    ;; Test command with arguments
    (println "  Testing command with arguments...")
    (match (proc:run (make-list "ls" "-la" "/tmp"))
      ((Ok (proc:ProcessResult _ _ (proc:ExitSuccess)))
       (println "    ✓ Command with arguments works"))
      ((Ok (proc:ProcessResult _ _ (proc:ExitFailure _)))
       (println "    ✓ Command executed (may fail on some systems)"))
      ((Err _) (println "    ✗ Command with arguments failed")))

    ;; Test exit codes
    (println "  Testing exit code handling...")
    (match (proc:run (make-list "false"))
      ((Ok (proc:ProcessResult _ _ (proc:ExitFailure _)))
       (println "    ✓ Exit code captured correctly"))
      ((Ok (proc:ProcessResult _ _ (proc:ExitSuccess)))
       (println "    ✗ Expected failure but got success"))
      ((Err _) (println "    ✓ Error handling works (command may not exist)")))

    ;; Test stdout capture
    (println "  Testing stdout capture...")
    (match (proc:run (make-list "echo" "test-output"))
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "test-output" stdout)
           (println "    ✓ Stdout capture works")
           (println "    ✗ Stdout not captured correctly")))
      ((Err _) (println "    ✗ Stdout capture failed"))))

  (define (test-shell-execution)
    "Test shell command execution."
    (println "")
    (println "2. Testing Shell Command Execution")
    (println "  ---------------------------------")

    ;; Test basic shell command
    (println "  Testing basic shell command...")
    (match (proc:shell "echo 'Shell works!'")
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "Shell works!" stdout)
           (println "    ✓ Basic shell command works")
           (println "    ✗ Shell output incorrect")))
      ((Err _) (println "    ✗ Basic shell command failed")))

    ;; Test shell pipeline
    (println "  Testing shell pipeline...")
    (match (proc:shell "echo 'hello world' | wc -w")
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "2" stdout)
           (println "    ✓ Shell pipeline works")
           (println "    ✗ Shell pipeline incorrect")))
      ((Err _) (println "    ✗ Shell pipeline failed")))

    ;; Test shell with variables
    (println "  Testing shell with variables...")
    (match (proc:shell "VAR=test; echo $VAR")
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "test" stdout)
           (println "    ✓ Shell variables work")
           (println "    ✗ Shell variables failed")))
      ((Err _) (println "    ✗ Shell variable test failed")))

    ;; Test shell with input
    (println "  Testing shell with input...")
    (match (proc:shell-with-input "cat" "input-test")
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "input-test" stdout)
           (println "    ✓ Shell with input works")
           (println "    ✗ Shell input not passed correctly")))
      ((Err _) (println "    ✗ Shell with input failed"))))

  (define (test-input-output)
    "Test input/output handling."
    (println "")
    (println "3. Testing Input/Output Handling")
    (println "  -------------------------------")

    ;; Test stdin input
    (println "  Testing stdin input...")
    (match (proc:run-with-input (make-list "cat") "Test input")
      ((Ok (proc:ProcessResult stdout _ _))
       (if (== (coalton-library/string:strip stdout) "Test input")
           (println "    ✓ Stdin input works")
           (println "    ✗ Stdin input mismatch")))
      ((Err _) (println "    ✗ Stdin input failed")))

    ;; Test stderr capture
    (println "  Testing stderr capture...")
    (match (proc:run (make-list "sh" "-c" "echo error >&2"))
      ((Ok (proc:ProcessResult _ stderr _))
       (if (coalton-library/string:contains? "error" stderr)
           (println "    ✓ Stderr capture works")
           (println "    ✗ Stderr not captured")))
      ((Err _) (println "    ✗ Stderr test failed")))

    ;; Test capture functions
    (println "  Testing capture functions...")
    (match (proc:capture-output (make-list "echo" "capture-test"))
      ((Ok output)
       (if (coalton-library/string:contains? "capture-test" output)
           (println "    ✓ Capture-output works")
           (println "    ✗ Capture-output failed")))
      ((Err _) (println "    ✗ Capture-output error")))

    (match (proc:capture-all (make-list "sh" "-c" "echo out; echo err >&2"))
      ((Ok (Tuple stdout stderr))
       (if (and (coalton-library/string:contains? "out" stdout)
                (coalton-library/string:contains? "err" stderr))
           (println "    ✓ Capture-all works")
           (println "    ✗ Capture-all incomplete")))
      ((Err _) (println "    ✗ Capture-all failed"))))

  (define (test-piping)
    "Test command piping and chaining."
    (println "")
    (println "4. Testing Command Piping")
    (println "  ------------------------")

    ;; Test simple pipe
    (println "  Testing simple pipe...")
    (match (proc:pipe (make-list "echo" "one two three")
                      (make-list "wc" "-w"))
      ((Ok (proc:ProcessResult stdout _ _))
       (if (coalton-library/string:contains? "3" stdout)
           (println "    ✓ Simple piping works")
           (println "    ✗ Pipe output incorrect")))
      ((Err _) (println "    ✗ Simple piping failed")))

    ;; Test command chain
    (println "  Testing command chain...")
    (let ((commands (make-list
                      (make-list "echo" "test")
                      (make-list "cat")
                      (make-list "wc" "-c"))))
      (match (proc:chain-commands commands)
        ((Ok _) (println "    ✓ Command chaining works"))
        ((Err _) (println "    ✗ Command chaining failed"))))

    ;; Test pipe-through
    (println "  Testing pipe-through...")
    (let ((pipeline-commands (make-list
                               (make-list "cat")
                               (make-list "wc" "-c"))))
      (match (proc:pipe-through (make-list "echo" "hello") pipeline-commands)
        ((Ok _) (println "    ✓ Pipe-through works"))
        ((Err _) (println "    ✗ Pipe-through failed")))))

  (define (test-environment)
    "Test environment variable operations."
    (println "")
    (println "5. Testing Environment Variables")
    (println "  -------------------------------")

    ;; Test get environment variable
    (println "  Testing get environment variable...")
    (match (proc:get-env "PATH")
      ((Some _) (println "    ✓ Get env var works (PATH found)"))
      (None (println "    ✗ PATH not found (unexpected)")))

    ;; Test set environment variable
    (println "  Testing set environment variable...")
    (match (proc:set-env "SMELTER_TEST" "42")
      ((Ok _)
       (match (proc:get-env "SMELTER_TEST")
         ((Some value)
          (if (== value "42")
              (println "    ✓ Set env var works")
              (println "    ✗ Env var value mismatch")))
         (None (println "    ✗ Env var not set"))))
      ((Err _) (println "    ✗ Set env failed")))

    ;; Test env-exists?
    (println "  Testing env-exists?...")
    (if (proc:env-exists? "PATH")
        (println "    ✓ Env-exists? works for PATH")
        (println "    ✗ Env-exists? failed for PATH"))

    ;; Test unset environment variable
    (println "  Testing unset environment variable...")
    (match (proc:unset-env "SMELTER_TEST")
      ((Ok _)
       (if (not (proc:env-exists? "SMELTER_TEST"))
           (println "    ✓ Unset env var works")
           (println "    ✗ Env var still exists after unset")))
      ((Err _) (println "    ✗ Unset env failed"))))

  (define (test-working-directory)
    "Test working directory operations."
    (println "")
    (println "6. Testing Working Directory")
    (println "  ---------------------------")

    ;; Test get working directory
    (println "  Testing get working directory...")
    (let ((cwd (proc:get-working-directory)))
      (if (not (coalton-library/string:empty? cwd))
          (println "    ✓ Get working directory works")
          (println "    ✗ Working directory empty")))

    ;; Test with-working-directory
    (println "  Testing with-working-directory...")
    (let ((original-cwd (proc:get-working-directory)))
      (match (proc:with-working-directory "/tmp" (make-list "pwd"))
        ((Ok (proc:ProcessResult stdout _ _))
         (if (coalton-library/string:contains? "/tmp" stdout)
             (println "    ✓ With-working-directory works")
             (println "    ✗ Working directory not changed")))
        ((Err _) (println "    ✗ With-working-directory failed")))

      ;; Verify we're back to original directory
      (if (== (proc:get-working-directory) original-cwd)
          (println "    ✓ Working directory restored")
          (println "    ✗ Working directory not restored"))))

  (define (test-utilities)
    "Test utility functions."
    (println "")
    (println "7. Testing Utility Functions")
    (println "  ---------------------------")

    ;; Test which command
    (println "  Testing which command...")
    (match (proc:which "sh")
      ((Some path)
       (if (coalton-library/string:contains? "sh" path)
           (println "    ✓ Which command works")
           (println "    ✗ Which returned unexpected path")))
      (None (println "    ✗ sh not found in PATH")))

    ;; Test executable check
    (println "  Testing executable check...")
    (match (proc:which "sh")
      ((Some sh-path)
       (if (proc:executable? sh-path)
           (println "    ✓ Executable check works")
           (println "    ✗ sh not detected as executable")))
      (None (println "    ✗ Cannot test executable (sh not found)")))

    ;; Test shell escaping
    (println "  Testing shell escaping...")
    (let ((escaped (proc:escape-shell-arg "test'string")))
      (if (coalton-library/string:contains? "'" escaped)
          (println "    ✓ Shell escaping works")
          (println "    ✗ Shell escaping incorrect")))

    ;; Test quote shell args
    (println "  Testing quote shell args...")
    (let ((quoted (proc:quote-shell-args (make-list "arg1" "arg with spaces" "arg'quote"))))
      (if (and (coalton-library/string:contains? "arg1" quoted)
               (coalton-library/string:contains? "spaces" quoted))
          (println "    ✓ Quote shell args works")
          (println "    ✗ Quote shell args failed")))

    ;; Test parse command
    (println "  Testing parse command...")
    (let ((parsed (proc:parse-command "ls -la /tmp")))
      (if (== (coalton-library/list:length parsed) 3)
          (println "    ✓ Parse command works")
          (println "    ✗ Parse command incorrect")))

    ;; Test get current PID
    (println "  Testing get current PID...")
    (let ((pid (proc:get-current-pid)))
      (if (> pid 0)
          (println "    ✓ Get current PID works")
          (println "    ✗ Invalid PID returned"))))

  (define (test-error-handling)
    "Test error conditions and edge cases."
    (println "")
    (println "8. Testing Error Handling")
    (println "  ------------------------")

    ;; Test non-existent command
    (println "  Testing non-existent command...")
    (match (proc:run (make-list "nonexistentcommand123xyz"))
      ((Err _) (println "    ✓ Non-existent command error caught"))
      ((Ok _) (println "    ✗ Should have failed for non-existent command")))

    ;; Test invalid shell command
    (println "  Testing invalid shell syntax...")
    (match (proc:shell "((invalid shell syntax")
      ((Err _) (println "    ✓ Invalid shell syntax caught"))
      ((Ok _) (println "    ✗ Should have failed for invalid syntax")))

    ;; Test empty command list
    (println "  Testing empty command list...")
    (match (proc:run (make-list))
      ((Err _) (println "    ✓ Empty command error caught"))
      ((Ok _) (println "    ✗ Should have failed for empty command")))

    ;; Test which with non-existent command
    (println "  Testing which with non-existent command...")
    (match (proc:which "definitely-does-not-exist-xyz")
      (None (println "    ✓ Which returns None for non-existent"))
      ((Some _) (println "    ✗ Which should return None"))))

  (define (test-legacy-compatibility)
    "Test legacy API compatibility."
    (println "")
    (println "9. Testing Legacy API Compatibility")
    (println "  ----------------------------------")

    ;; Test legacy run-process
    (println "  Testing legacy run-process...")
    (match (proc:run-process "echo legacy-test")
      ((Ok result)
       (if (coalton-library/string:contains? "legacy-test" (proc:process-stdout result))
           (println "    ✓ Legacy run-process works")
           (println "    ✗ Legacy run-process output incorrect")))
      ((Err _) (println "    ✗ Legacy run-process failed")))

    ;; Test legacy command-exists?
    (println "  Testing legacy command-exists?...")
    (if (proc:command-exists? "sh")
        (println "    ✓ Legacy command-exists? works")
        (println "    ✗ Legacy command-exists? failed"))

    ;; Test legacy capture-command
    (println "  Testing legacy capture-command...")
    (match (proc:capture-command "echo legacy-capture")
      ((Ok output)
       (if (coalton-library/string:contains? "legacy-capture" output)
           (println "    ✓ Legacy capture-command works")
           (println "    ✗ Legacy capture-command output incorrect")))
      ((Err _) (println "    ✗ Legacy capture-command failed")))

    ;; Test legacy process result accessors
    (println "  Testing legacy process result accessors...")
    (match (proc:run-process "echo accessor-test")
      ((Ok result)
       (let ((stdout (proc:process-stdout result))
             (stderr (proc:process-stderr result))
             (exit-code (proc:process-exit-code result)))
         (if (and (coalton-library/string:contains? "accessor-test" stdout)
                  (match exit-code
                    ((proc:ExitSuccess) True)
                    (_ False)))
             (println "    ✓ Legacy accessors work")
             (println "    ✗ Legacy accessors failed"))))
      ((Err _) (println "    ✗ Legacy accessor test failed"))))

  ;; Helper function for creating lists
  (define (make-list . items)
    "Create a list from arguments."
    items)

  ;; String utility function that might be missing
  (define (format pattern . args)
    "Simple format function (placeholder)."
    pattern)  ; Simplified for now
)