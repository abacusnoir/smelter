#!/bin/bash

set -e

echo "=== Smelter Filesystem Adapter Integration Tests ==="
echo

# Ensure we're in the right directory
if [ ! -f "./smt" ]; then
    echo "❌ Error: smt executable not found. Please run 'make build' first."
    exit 1
fi

echo "✓ Found smt executable"

# Test 1: Basic file operations
echo "🧪 Test 1: Basic file operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (write-file "/tmp/test-basic.txt" "Hello Filesystem")
    ((Ok _) (match (read-file "/tmp/test-basic.txt")
              ((Ok content)
               (progn
                 (delete-file "/tmp/test-basic.txt")
                 (println content)
                 True))
              ((Err e) False)))
    ((Err e) False)))'

# Test 2: File existence checks
echo "🧪 Test 2: File existence checks"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (write-file "/tmp/test-exists.txt" "exists test")
    ((Ok _)
     (let ((exists-before (file-exists? "/tmp/test-exists.txt")))
       (match (delete-file "/tmp/test-exists.txt")
         ((Ok _)
          (let ((exists-after (file-exists? "/tmp/test-exists.txt")))
            (println (if (and exists-before (not exists-after))
                        "✓ File existence test passed"
                        "✗ File existence test failed"))))
         ((Err e) (println "✗ Delete failed")))))
    ((Err e) (println "✗ Write failed"))))'

# Test 3: Directory operations
echo "🧪 Test 3: Directory operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (create-directory "/tmp/test-dir-smelter")
    ((Ok _)
     (let ((is-dir (directory-exists? "/tmp/test-dir-smelter")))
       (match (remove-directory "/tmp/test-dir-smelter")
         ((Ok _)
          (let ((removed (not (directory-exists? "/tmp/test-dir-smelter"))))
            (println (if (and is-dir removed)
                        "✓ Directory operations test passed"
                        "✗ Directory operations test failed"))))
         ((Err e) (println "✗ Remove directory failed")))))
    ((Err e) (println "✗ Create directory failed"))))'

# Test 4: File copy operations
echo "🧪 Test 4: File copy operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (write-file "/tmp/test-copy-src.txt" "copy test content")
    ((Ok _)
     (match (copy-file "/tmp/test-copy-src.txt" "/tmp/test-copy-dest.txt")
       ((Ok _)
        (match (read-file "/tmp/test-copy-dest.txt")
          ((Ok content)
           (progn
             (delete-file "/tmp/test-copy-src.txt")
             (delete-file "/tmp/test-copy-dest.txt")
             (println (if (== content "copy test content")
                         "✓ File copy test passed"
                         "✗ File copy test failed"))))
          ((Err e) (println "✗ Read dest file failed"))))
       ((Err e) (println "✗ Copy failed"))))
    ((Err e) (println "✗ Write source failed"))))'

# Test 5: Byte operations
echo "🧪 Test 5: Byte operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (let ((test-bytes (make-list 72 101 108 108 111))) ; "Hello" in ASCII
    (match (write-file-bytes "/tmp/test-bytes.bin" test-bytes)
      ((Ok _)
       (match (read-file-bytes "/tmp/test-bytes.bin")
         ((Ok bytes)
          (progn
            (delete-file "/tmp/test-bytes.bin")
            (println (if (== bytes test-bytes)
                        "✓ Byte operations test passed"
                        "✗ Byte operations test failed"))))
         ((Err e) (println "✗ Read bytes failed"))))
      ((Err e) (println "✗ Write bytes failed")))))'

# Test 6: Line operations
echo "🧪 Test 6: Line operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (let ((test-lines (make-list "Line 1" "Line 2" "Line 3")))
    (match (write-lines "/tmp/test-lines.txt" test-lines)
      ((Ok _)
       (match (read-lines "/tmp/test-lines.txt")
         ((Ok lines)
          (progn
            (delete-file "/tmp/test-lines.txt")
            (println (if (== lines test-lines)
                        "✓ Line operations test passed"
                        "✗ Line operations test failed"))))
         ((Err e) (println "✗ Read lines failed"))))
      ((Err e) (println "✗ Write lines failed")))))'

# Test 7: Path utilities
echo "🧪 Test 7: Path utilities"
./smt eval '(progn
  (use smelter/adapters/fs)
  (let ((cwd (current-directory))
        (filename (path-filename "test.txt"))
        (extension (path-extension "test.txt")))
    (println (if (and (not (== cwd ""))
                      (== filename "test.txt")
                      (== extension "txt"))
                "✓ Path utilities test passed"
                "✗ Path utilities test failed"))))'

# Test 8: Error handling
echo "🧪 Test 8: Error handling"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (read-file "/tmp/this-file-absolutely-does-not-exist-smelter.txt")
    ((Ok _) (println "✗ Error handling test failed - should have failed"))
    ((Err (FileNotFound _)) (println "✓ Error handling test passed"))
    ((Err e) (println "✗ Error handling test failed - wrong error type"))))'

# Test 9: File info and size
echo "🧪 Test 9: File info and size"
./smt eval '(progn
  (use smelter/adapters/fs)
  (let ((test-content "File size test content"))
    (match (write-file "/tmp/test-size.txt" test-content)
      ((Ok _)
       (match (file-size "/tmp/test-size.txt")
         ((Ok size)
          (progn
            (delete-file "/tmp/test-size.txt")
            (println (if (== size (into (string-length test-content)))
                        "✓ File size test passed"
                        "✗ File size test failed"))))
         ((Err e) (println "✗ File size failed"))))
      ((Err e) (println "✗ Write for size test failed")))))'

# Test 10: Move/rename operations
echo "🧪 Test 10: Move/rename operations"
./smt eval '(progn
  (use smelter/adapters/fs)
  (match (write-file "/tmp/test-move-src.txt" "move test")
    ((Ok _)
     (match (move-file "/tmp/test-move-src.txt" "/tmp/test-move-dest.txt")
       ((Ok _)
        (let ((src-gone (not (file-exists? "/tmp/test-move-src.txt")))
              (dest-exists (file-exists? "/tmp/test-move-dest.txt")))
          (progn
            (delete-file "/tmp/test-move-dest.txt")
            (println (if (and src-gone dest-exists)
                        "✓ Move operations test passed"
                        "✗ Move operations test failed")))))
       ((Err e) (println "✗ Move failed"))))
    ((Err e) (println "✗ Write for move test failed"))))'

echo
echo "=== Integration Tests Complete ==="
echo "Run comprehensive unit tests with: ./smt run test/adapters/fs-test.lisp"
echo "Run existing adapter tests with: ./smt run test/adapter-tests.coal"