#!/usr/bin/env smt run

(coalton-toplevel
  (use smelter.stdlib.test)
  (use smelter/adapters/fs)

  ;; Helper function to cleanup test files
  (declare cleanup-test-file (String -> Unit))
  (define (cleanup-test-file path)
    "Silently delete a test file if it exists"
    (match (delete-file path)
      ((Ok _) Unit)
      ((Err _) Unit)))

  ;; Test basic file read/write operations
  (declare test-basic-file-operations (Unit -> TestResult))
  (define test-basic-file-operations
    (test-case "Basic File Operations"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-basic.txt")
              (test-content "Hello, Filesystem Adapter!"))
          (progn
            (cleanup-test-file test-file)
            (match (write-file test-file test-content)
              ((Ok _)
               (match (read-file test-file)
                 ((Ok content)
                  (progn
                    (cleanup-test-file test-file)
                    (== content test-content)))
                 (_ False)))
              (_ False)))))))

  ;; Test file existence checks
  (declare test-file-existence (Unit -> TestResult))
  (define test-file-existence
    (test-case "File Existence Checks"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-exists.txt"))
          (progn
            (cleanup-test-file test-file)
            (match (file-exists? test-file)
              ((Ok False)
               (match (write-file test-file "test")
                 ((Ok _)
                  (match (file-exists? test-file)
                    ((Ok True)
                     (match (is-file? test-file)
                       ((Ok True)
                        (match (is-directory? test-file)
                          ((Ok False)
                           (progn
                             (cleanup-test-file test-file)
                             (match (file-exists? test-file)
                               ((Ok False) True)
                               (_ False))))
                          (_ False)))
                       (_ False)))
                    (_ False)))
                 (_ False)))
              (_ False)))))))

  ;; Test byte operations
  (declare test-byte-operations (Unit -> TestResult))
  (define test-byte-operations
    (test-case "Byte Operations"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-bytes.bin")
              (test-bytes (make-list 72 101 108 108 111))) ; "Hello" in ASCII
          (progn
            (cleanup-test-file test-file)
            (match (write-file-bytes test-file test-bytes)
              ((Ok _)
               (match (read-file-bytes test-file)
                 ((Ok bytes)
                  (progn
                    (cleanup-test-file test-file)
                    (== bytes test-bytes)))
                 (_ False)))
              (_ False)))))))

  ;; Test file copy operations
  (declare test-file-copy (Unit -> TestResult))
  (define test-file-copy
    (test-case "File Copy Operations"
      (fn ()
        (let ((source-file "/tmp/smelter-fs-test-source.txt")
              (dest-file "/tmp/smelter-fs-test-dest.txt")
              (test-content "Copy test content"))
          (progn
            (cleanup-test-file source-file)
            (cleanup-test-file dest-file)
            (match (write-file source-file test-content)
              ((Ok _)
               (match (copy-file source-file dest-file)
                 ((Ok _)
                  (match (read-file dest-file)
                    ((Ok content)
                     (progn
                       (cleanup-test-file source-file)
                       (cleanup-test-file dest-file)
                       (== content test-content)))
                    (_ False)))
                 (_ False)))
              (_ False)))))))

  ;; Test file move/rename operations
  (declare test-file-move (Unit -> TestResult))
  (define test-file-move
    (test-case "File Move Operations"
      (fn ()
        (let ((source-file "/tmp/smelter-fs-test-move-source.txt")
              (dest-file "/tmp/smelter-fs-test-move-dest.txt")
              (test-content "Move test content"))
          (progn
            (cleanup-test-file source-file)
            (cleanup-test-file dest-file)
            (match (write-file source-file test-content)
              ((Ok _)
               (match (move-file source-file dest-file)
                 ((Ok _)
                  (match (file-exists? source-file)
                    ((Ok False)
                     (match (file-exists? dest-file)
                       ((Ok True)
                        (match (read-file dest-file)
                          ((Ok content)
                           (progn
                             (cleanup-test-file dest-file)
                             (== content test-content)))
                          (_ False)))
                       (_ False)))
                    (_ False)))
                 (_ False)))
              (_ False)))))))

  ;; Test directory operations
  (declare test-directory-operations (Unit -> TestResult))
  (define test-directory-operations
    (test-case "Directory Operations"
      (fn ()
        (let ((test-dir "/tmp/smelter-fs-test-dir"))
          (progn
            (match (create-directory test-dir)
              ((Ok _)
               (match (directory-exists? test-dir)
                 ((Ok True)
                  (match (is-directory? test-dir)
                    ((Ok True)
                     (match (is-file? test-dir)
                       ((Ok False)
                        (match (remove-directory test-dir)
                          ((Ok _)
                           (match (directory-exists? test-dir)
                             ((Ok False) True)
                             (_ False)))
                          (_ False)))
                       (_ False)))
                    (_ False)))
                 (_ False)))
              (_ False)))))))

  ;; Test file info operations
  (declare test-file-info (Unit -> TestResult))
  (define test-file-info
    (test-case "File Info Operations"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-info.txt")
              (test-content "File info test content"))
          (progn
            (cleanup-test-file test-file)
            (match (write-file test-file test-content)
              ((Ok _)
               (match (get-file-info test-file)
                 ((Ok (FileInfo size _ is-dir _))
                  (progn
                    (cleanup-test-file test-file)
                    (and (> size 0) (not is-dir))))
                 (_ False)))
              (_ False)))))))

  ;; Test file size operation
  (declare test-file-size (Unit -> TestResult))
  (define test-file-size
    (test-case "File Size Operation"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-size.txt")
              (test-content "Size test"))
          (progn
            (cleanup-test-file test-file)
            (match (write-file test-file test-content)
              ((Ok _)
               (match (file-size test-file)
                 ((Ok size)
                  (progn
                    (cleanup-test-file test-file)
                    (== size (into (string-length test-content)))))
                 (_ False)))
              (_ False)))))))

  ;; Test line operations
  (declare test-line-operations (Unit -> TestResult))
  (define test-line-operations
    (test-case "Line Operations"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-lines.txt")
              (test-lines (make-list "First line" "Second line" "Third line")))
          (progn
            (cleanup-test-file test-file)
            (match (write-lines test-file test-lines)
              ((Ok _)
               (match (read-lines test-file)
                 ((Ok lines)
                  (progn
                    (cleanup-test-file test-file)
                    (== lines test-lines)))
                 (_ False)))
              (_ False)))))))

  ;; Test path utilities
  (declare test-path-utilities (Unit -> TestResult))
  (define test-path-utilities
    (test-case "Path Utilities"
      (fn ()
        (let ((path1 "/tmp")
              (path2 "test-file.txt")
              (full-path (join-paths path1 path2)))
          (match (current-directory)
            ((Ok cwd)
             (and (not (== cwd ""))
                  (== (path-filename "test.txt") "test.txt")
                  (== (path-extension "test.txt") "txt")))
            (_ False))))))

  ;; Test append operations
  (declare test-append-operations (Unit -> TestResult))
  (define test-append-operations
    (test-case "Append Operations"
      (fn ()
        (let ((test-file "/tmp/smelter-fs-test-append.txt")
              (initial-content "Initial content\n")
              (additional-content "Additional content"))
          (progn
            (cleanup-test-file test-file)
            (match (write-file test-file initial-content)
              ((Ok _)
               (match (append-file test-file additional-content)
                 ((Ok _)
                  (match (read-file test-file)
                    ((Ok content)
                     (progn
                       (cleanup-test-file test-file)
                       (== content (mconcat (make-list initial-content additional-content)))))
                    (_ False)))
                 (_ False)))
              (_ False)))))))

  ;; Test error handling for non-existent files
  (declare test-error-handling (Unit -> TestResult))
  (define test-error-handling
    (test-case "Error Handling"
      (fn ()
        (let ((nonexistent-file "/tmp/this-file-does-not-exist-smelter-test.txt"))
          (progn
            (cleanup-test-file nonexistent-file)
            (match (read-file nonexistent-file)
              ((Err (FileNotFound _)) True)
              (_ False)))))))

  ;; Test temporary file utilities
  (declare test-temp-utilities (Unit -> TestResult))
  (define test-temp-utilities
    (test-case "Temporary Utilities"
      (fn ()
        (match (with-temp-file (fn (temp-path)
                                   (match (write-file temp-path "temp test")
                                     ((Ok _) (Ok "Temp file test successful"))
                                     ((Err e) (Err e)))))
          ((Ok _) True)
          (_ False)))))

  ;; Main test runner
  (define main
    (run-test-suite "Smelter Filesystem Adapter Tests"
      (make-list
       test-basic-file-operations
       test-file-existence
       test-byte-operations
       test-file-copy
       test-file-move
       test-directory-operations
       test-file-info
       test-file-size
       test-line-operations
       test-path-utilities
       test-append-operations
       test-error-handling
       test-temp-utilities))))