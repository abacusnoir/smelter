# Smelter Filesystem Adapter

The Smelter Filesystem Adapter provides type-safe, functional filesystem operations for Coalton scripts. All operations return `Result` types for proper error handling and follow Coalton functional programming idioms.

## Installation

The filesystem adapter is built into Smelter core and requires no additional installation.

## Usage

The filesystem adapter is available through the `smelter/adapters/fs` namespace:

```coalton
(coalton-toplevel
  (define example-usage
    (match (smelter/adapters/fs:write-file "/tmp/example.txt" "Hello, World!")
      ((Ok _)
       (match (smelter/adapters/fs:read-file "/tmp/example.txt")
         ((Ok content)
          (progn
            (smelter/adapters/fs:delete-file "/tmp/example.txt")
            content))
         ((Err e) "Read failed")))
      ((Err e) "Write failed"))))
```

### Eval Mode

For quick operations in eval mode:

```bash
# Write a file
./smt eval '(smelter/adapters/fs:write-file "/tmp/test.txt" "content")'

# Read a file
./smt eval '(smelter/adapters/fs:read-file "/tmp/test.txt")'

# Check if file exists
./smt eval '(smelter/adapters/fs:file-exists? "/etc/hosts")'

# Get current directory
./smt eval '(smelter/adapters/fs:current-directory)'
```

## Types

### FileInfo

Represents file metadata:

```coalton
(define-type FileInfo
  (FileInfo Integer Integer Boolean String))
  ; size, modified-time, is-directory, path
```

### FSError

Comprehensive error types for filesystem operations:

```coalton
(define-type FSError
  (FileNotFound String)
  (PermissionDenied String)
  (IOError String)
  (FileSystemError String))
```

## API Reference

### File Reading Operations

#### `read-file`
```coalton
(declare read-file (String -> (Result FSError String)))
```
Read entire file contents as a string.

**Example:**
```coalton
(match (smelter/adapters/fs:read-file "/path/to/file.txt")
  ((Ok content) (println content))
  ((Err (FileNotFound path)) (println "File not found"))
  ((Err e) (println "Other error")))
```

#### `read-file-bytes`
```coalton
(declare read-file-bytes (String -> (Result FSError (List Integer))))
```
Read entire file contents as a list of bytes.

**Example:**
```coalton
(match (smelter/adapters/fs:read-file-bytes "/path/to/binary.bin")
  ((Ok bytes) (println "Read bytes successfully"))
  ((Err e) (println "Read failed")))
```

#### `read-lines`
```coalton
(declare read-lines (String -> (Result FSError (List String))))
```
Read file as a list of lines.

**Example:**
```coalton
(match (smelter/adapters/fs:read-lines "/path/to/file.txt")
  ((Ok lines) (fold (fn (acc line) (println line)) Unit lines))
  ((Err e) (println "Read lines failed")))
```

### File Writing Operations

#### `write-file`
```coalton
(declare write-file (String -> String -> (Result FSError Unit)))
```
Write string content to file (overwrites existing).

**Example:**
```coalton
(match (smelter/adapters/fs:write-file "/tmp/output.txt" "Hello, World!")
  ((Ok _) (println "Write successful"))
  ((Err e) (println "Write failed")))
```

#### `write-file-bytes`
```coalton
(declare write-file-bytes (String -> (List Integer) -> (Result FSError Unit)))
```
Write list of bytes to file (overwrites existing).

**Example:**
```coalton
(let ((bytes (make-list 72 101 108 108 111))) ; "Hello" in ASCII
  (smelter/adapters/fs:write-file-bytes "/tmp/binary.bin" bytes))
```

#### `write-lines`
```coalton
(declare write-lines (String -> (List String) -> (Result FSError Unit)))
```
Write list of lines to file.

**Example:**
```coalton
(let ((lines (make-list "Line 1" "Line 2" "Line 3")))
  (smelter/adapters/fs:write-lines "/tmp/lines.txt" lines))
```

#### `append-file`
```coalton
(declare append-file (String -> String -> (Result FSError Unit)))
```
Append string content to file.

**Example:**
```coalton
(smelter/adapters/fs:append-file "/tmp/log.txt" "New log entry\n")
```

### File Operations

#### `delete-file`
```coalton
(declare delete-file (String -> (Result FSError Unit)))
```
Delete a file.

#### `copy-file`
```coalton
(declare copy-file (String -> String -> (Result FSError Unit)))
```
Copy file from source to destination.

**Example:**
```coalton
(smelter/adapters/fs:copy-file "/tmp/source.txt" "/tmp/destination.txt")
```

#### `move-file`
```coalton
(declare move-file (String -> String -> (Result FSError Unit)))
```
Move file from source to destination.

#### `rename-file`
```coalton
(declare rename-file (String -> String -> (Result FSError Unit)))
```
Rename file (alias for move-file).

### File Queries

#### `file-exists?`
```coalton
(declare file-exists? (String -> Boolean))
```
Check if file exists.

#### `directory-exists?`
```coalton
(declare directory-exists? (String -> Boolean))
```
Check if directory exists.

#### `is-file?`
```coalton
(declare is-file? (String -> Boolean))
```
Check if path exists and is a regular file.

#### `is-directory?`
```coalton
(declare is-directory? (String -> Boolean))
```
Check if path exists and is a directory.

#### `get-file-info`
```coalton
(declare get-file-info (String -> (Result FSError FileInfo)))
```
Get comprehensive file information.

**Example:**
```coalton
(match (smelter/adapters/fs:get-file-info "/path/to/file.txt")
  ((Ok (FileInfo size modified-time is-dir path))
   (if is-dir
       (println "It's a directory")
       (println (mconcat (make-list "File size: " (into size))))))
  ((Err e) (println "Could not get file info")))
```

#### `file-size`
```coalton
(declare file-size (String -> (Result FSError Integer)))
```
Get file size in bytes.

### Directory Operations

#### `list-directory`
```coalton
(declare list-directory (String -> (Result FSError (List String))))
```
List directory contents.

**Example:**
```coalton
(match (smelter/adapters/fs:list-directory "/tmp")
  ((Ok files)
   (fold (fn (acc file) (println file)) Unit files))
  ((Err e) (println "Directory listing failed")))
```

#### `create-directory`
```coalton
(declare create-directory (String -> (Result FSError Unit)))
```
Create directory (and parents if needed).

#### `create-directories`
```coalton
(declare create-directories (String -> (Result FSError Unit)))
```
Create directory and parents if needed (alias for create-directory).

#### `remove-directory`
```coalton
(declare remove-directory (String -> (Result FSError Unit)))
```
Remove directory and its contents.

### Path Utilities

#### `current-directory`
```coalton
(declare current-directory (Unit -> String))
```
Get current working directory.

**Example:**
```coalton
(let ((cwd (smelter/adapters/fs:current-directory)))
  (println (mconcat (make-list "Current directory: " cwd))))
```

#### `join-paths`
```coalton
(declare join-paths (String -> String -> String))
```
Join two path components.

**Example:**
```coalton
(let ((full-path (smelter/adapters/fs:join-paths "/tmp" "example.txt")))
  (println full-path)) ; "/tmp/example.txt"
```

#### `absolute-path`
```coalton
(declare absolute-path (String -> String))
```
Get absolute path.

#### `path-filename`
```coalton
(declare path-filename (String -> String))
```
Get filename from path.

**Example:**
```coalton
(let ((filename (smelter/adapters/fs:path-filename "/path/to/file.txt")))
  (println filename)) ; "file.txt"
```

#### `path-extension`
```coalton
(declare path-extension (String -> String))
```
Get file extension.

**Example:**
```coalton
(let ((ext (smelter/adapters/fs:path-extension "document.pdf")))
  (println ext)) ; "pdf"
```

### Temporary File Utilities

#### `with-temp-file`
```coalton
(declare with-temp-file ((String -> Boolean) -> Boolean))
```
Execute function with a temporary file path. The temporary file is automatically cleaned up.

**Example:**
```coalton
(let ((result (smelter/adapters/fs:with-temp-file
                (fn (temp-path)
                  (match (smelter/adapters/fs:write-file temp-path "temp content")
                    ((Ok _) True)
                    ((Err _) False))))))
  result) ; True if write succeeded, file is automatically deleted
```

#### `with-temp-directory`
```coalton
(declare with-temp-directory ((String -> Boolean) -> Boolean))
```
Execute function with a temporary directory path. The temporary directory is automatically cleaned up.

## Error Handling

All filesystem operations that can fail return `Result` types. Always handle errors appropriately:

```coalton
(match (smelter/adapters/fs:read-file "/path/to/file.txt")
  ((Ok content)
   ; Handle success case
   (println content))
  ((Err (FileNotFound path))
   ; Handle file not found
   (println (mconcat (make-list "File not found: " path))))
  ((Err (PermissionDenied path))
   ; Handle permission denied
   (println (mconcat (make-list "Permission denied: " path))))
  ((Err (IOError msg))
   ; Handle I/O errors
   (println (mconcat (make-list "I/O error: " msg))))
  ((Err (FileSystemError msg))
   ; Handle general filesystem errors
   (println (mconcat (make-list "Filesystem error: " msg)))))
```

## Common Patterns

### Safe File Processing

```coalton
(define (process-file path)
  "Safely process a file with error handling"
  (match (smelter/adapters/fs:read-file path)
    ((Ok content)
     (let ((processed (process-content content)))
       (match (smelter/adapters/fs:write-file (mconcat (make-list path ".processed")) processed)
         ((Ok _) "Processing successful")
         ((Err e) "Failed to write processed file"))))
    ((Err e) "Failed to read input file")))
```

### Directory Traversal

```coalton
(define (list-all-files dir)
  "List all files in a directory"
  (match (smelter/adapters/fs:list-directory dir)
    ((Ok files)
     (filter (fn (file) (smelter/adapters/fs:is-file? file)) files))
    ((Err e) (make-list))))
```

### Atomic File Updates

```coalton
(define (atomic-update path new-content)
  "Atomically update a file using temporary file"
  (smelter/adapters/fs:with-temp-file
    (fn (temp-path)
      (match (smelter/adapters/fs:write-file temp-path new-content)
        ((Ok _)
         (match (smelter/adapters/fs:move-file temp-path path)
           ((Ok _) True)
           ((Err e) False)))
        ((Err e) False)))))
```

## Performance Notes

- File operations are synchronous and block until completion
- Large files are read/written entirely into memory
- For optimal performance with large files, consider processing in chunks
- Directory operations on large directories may take time
- Use `file-exists?` before expensive operations when appropriate

## Security Considerations

- All file operations respect filesystem permissions
- No automatic path sanitization is performed - validate paths in your application
- Temporary files are created with standard system permissions
- Always handle permission errors appropriately in user-facing applications

## Compatibility

- Compatible with all SBCL-supported platforms (Linux, macOS, Windows)
- Uses UIOP for cross-platform filesystem operations
- Path separators are handled automatically by the underlying system
- UTF-8 encoding is used for all text operations