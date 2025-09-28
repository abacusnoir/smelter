(defpackage #:smelter/adapters/fs
  (:documentation "
Smelter Filesystem Adapter

A comprehensive, type-safe filesystem adapter for Smelter that provides functional
access to file and directory operations. This adapter serves as the reference
implementation for all Smelter adapters, demonstrating best practices for:

- Type safety with Result types for all fallible operations
- Robust error handling with specific error conditions
- Cross-platform compatibility using UIOP utilities
- Functional programming patterns with immutable data

Design Philosophy:
- All fallible operations return (Result FSError T) to force error handling
- File paths are strings, leveraging UIOP for portability
- Operations are atomic where possible, with explicit side effects
- Temporary resources are automatically managed with cleanup guarantees

Key Features:
- File I/O: read-file, write-file, append-file with text and binary support
- Directory operations: create, remove, list with recursive capabilities
- File queries: existence, type, size, and metadata with Result safety
- Path utilities: joining, resolution, and filename extraction
- Temporary resources: automatic cleanup for temp files and directories

Error Handling:
- FileNotFound: Path does not exist
- PermissionDenied: Insufficient access rights
- IOError: General I/O failures (device full, network issues)
- FileSystemError: System-level errors

Usage:
  (use smelter/adapters/fs)

  (match (read-file \"config.txt\")
    ((Ok content) (process-config content))
    ((Err (FileNotFound path)) (create-default-config path))
    ((Err e) (handle-error e)))
")
  (:use #:coalton #:coalton-prelude)
  (:export
   #:FileInfo
   #:FSError #:FileNotFound #:PermissionDenied #:IOError #:FileSystemError
   #:read-file #:read-file-bytes #:read-lines
   #:write-file #:write-file-bytes #:append-file #:write-lines
   #:delete-file #:copy-file #:move-file #:rename-file
   #:file-exists? #:directory-exists? #:is-file? #:is-directory?
   #:get-file-info #:file-size
   #:list-directory #:create-directory #:create-directories #:remove-directory
   #:join-paths #:absolute-path #:current-directory
   #:path-filename #:path-extension
   #:with-temp-file #:with-temp-directory))

(in-package #:smelter/adapters/fs)

(coalton-toplevel
  (define-type FileInfo
    (FileInfo Integer Integer Boolean String))  ; size, modified-time, is-directory, path

  (define-type FSError
    (FileNotFound String)
    (PermissionDenied String)
    (IOError String)
    (FileSystemError String))



  ;; File reading and writing
  (declare read-file (String -> (Result FSError String)))
  (define (read-file path)
    "Read entire file contents as string.

    Loads the entire file into memory - not suitable for very large files.
    Returns (Err (FileNotFound path)) if file doesn't exist.
    Returns (Err (PermissionDenied path)) if file cannot be read due to permissions.

    Note: This operation may return an error instead of (Ok False) for permission
    issues, which allows distinguishing between 'file does not exist' and
    'file exists but cannot be read'."
    (lisp (Result FSError String) (path)
      (cl:handler-case
          (cl:with-open-file (stream path :direction :input :if-does-not-exist :error)
            (Ok (cl:with-output-to-string (s)
                  (cl:loop for line = (cl:read-line stream cl:nil cl:nil)
                           while line
                           do (cl:write-line line s)))))
        (cl:file-error (e)
          (Err (classify-file-error path e)))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Read error: ~A" e)))))))

  (declare write-file (String -> String -> (Result FSError Unit)))
  (define (write-file path content)
    "Write string content to file, overwriting existing content.

    Side effects:
    - Creates parent directories automatically if they don't exist
    - Overwrites existing file content completely
    - Creates file if it doesn't exist

    Returns (Err (PermissionDenied path)) if cannot write due to permissions."
    (lisp (Result FSError Unit) (path content)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :if-exists :supersede)
              (cl:write-string content stream))
            (Ok Unit))
        (cl:file-error (e)
          (Err (classify-file-error path e)))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Write error: ~A" e)))))))

  (declare append-file (String -> String -> (Result FSError Unit)))
  (define (append-file path content)
    "Append string content to file"
    (lisp (Result FSError Unit) (path content)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
              (cl:write-string content stream))
            (Ok Unit))
        (cl:file-error (e)
          (Err (IOError (cl:format cl:nil "Append error: ~A" e))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Append error: ~A" e)))))))

  (declare delete-file (String -> (Result FSError Unit)))
  (define (delete-file path)
    "Delete file"
    (lisp (Result FSError Unit) (path)
      (cl:handler-case
          (cl:progn
            (cl:delete-file path)
            (Ok Unit))
        (cl:file-error (e)
          (Err (classify-file-error path e)))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Delete error: ~A" e)))))))

  ;; File/directory existence checks
  (declare file-exists? (String -> (Result FSError Boolean)))
  (define (file-exists? path)
    "Check if path exists and is a regular file (not a directory).

    Returns:
    - (Ok True) if path exists and is a regular file
    - (Ok False) if path doesn't exist or is a directory
    - (Err (PermissionDenied path)) if path exists but cannot be accessed

    Note: Permission errors return Err instead of Ok(False) to distinguish
    between 'file does not exist' and 'file exists but inaccessible'."
    (lisp (Result FSError Boolean) (path)
      (cl:handler-case
          (cl:let ((probe-result (cl:probe-file path)))
            (Ok (cl:and probe-result
                        (cl:not (uiop:directory-pathname-p probe-result)))))
        (cl:file-error (e)
          (cl:let ((error-msg (cl:format cl:nil "~A" e)))
            (cl:cond
              ;; Use errno codes when available (more reliable than messages)
              ((cl:or (cl:search "ENOENT" error-msg)
                      (cl:search "No such file" error-msg)
                      (cl:search "does not exist" error-msg))
               (Ok False))  ; File doesn't exist is not an error, just False
              ((cl:or (cl:search "EACCES" error-msg)
                      (cl:search "EPERM" error-msg)
                      (cl:search "Permission denied" error-msg)
                      (cl:search "permission" error-msg))
               (Err (PermissionDenied (cl:format cl:nil "Cannot access: ~A" path))))
              (cl:t (Err (IOError (cl:format cl:nil "File check error: ~A" error-msg)))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "File check error: ~A" e)))))))

  (declare directory-exists? (String -> (Result FSError Boolean)))
  (define (directory-exists? path)
    "Check if path exists and is a directory.

    Returns:
    - (Ok True) if path exists and is a directory
    - (Ok False) if path doesn't exist or is a regular file
    - (Err (PermissionDenied path)) if path exists but cannot be accessed

    Note: Permission errors return Err instead of Ok(False) to distinguish
    between 'directory does not exist' and 'directory exists but inaccessible'."
    (lisp (Result FSError Boolean) (path)
      (cl:handler-case
          (cl:let ((probe-result (cl:probe-file path)))
            (Ok (cl:and probe-result
                        (uiop:directory-pathname-p probe-result))))
        (cl:file-error (e)
          (cl:let ((error-msg (cl:format cl:nil "~A" e)))
            (cl:cond
              ;; Use errno codes when available (more reliable than messages)
              ((cl:or (cl:search "ENOENT" error-msg)
                      (cl:search "No such file" error-msg)
                      (cl:search "does not exist" error-msg))
               (Ok False))  ; Directory doesn't exist is not an error, just False
              ((cl:or (cl:search "EACCES" error-msg)
                      (cl:search "EPERM" error-msg)
                      (cl:search "Permission denied" error-msg)
                      (cl:search "permission" error-msg))
               (Err (PermissionDenied (cl:format cl:nil "Cannot access: ~A" path))))
              (cl:t (Err (IOError (cl:format cl:nil "Directory check error: ~A" error-msg)))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Directory check error: ~A" e)))))))

  ;; File information - simplified version that doesn't rely on sb-posix
  (declare get-file-info (String -> (Result FSError FileInfo)))
  (define (get-file-info path)
    "Get file information"
    (lisp (Result FSError FileInfo) (path)
      (cl:handler-case
          (cl:let* ((truename (cl:truename path))
                    (write-date (cl:file-write-date truename))
                    (is-dir (uiop:directory-pathname-p truename))
                    (size (cl:if is-dir 
                                 0 
                                 (cl:with-open-file (stream truename)
                                   (cl:file-length stream)))))
            (Ok (FileInfo size write-date is-dir (cl:namestring truename))))
        (cl:file-error (e)
          (cl:let ((error-msg (cl:format cl:nil "~A" e)))
            (cl:cond
              ;; Use errno codes when available (more reliable than messages)
              ((cl:or (cl:search "ENOENT" error-msg)
                      (cl:search "No such file" error-msg)
                      (cl:search "does not exist" error-msg))
               (Err (FileNotFound path)))
              ((cl:or (cl:search "EACCES" error-msg)
                      (cl:search "EPERM" error-msg)
                      (cl:search "Permission denied" error-msg)
                      (cl:search "permission" error-msg))
               (Err (PermissionDenied path)))
              (cl:t (Err (IOError (cl:format cl:nil "File info error: ~A" error-msg)))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Stat error: ~A" e)))))))

  ;; Directory operations
  (declare list-directory (String -> (Result FSError (List String))))
  (define (list-directory path)
    "List directory contents"
    (lisp (Result FSError (List String)) (path)
      (cl:handler-case
          (cl:let ((entries (uiop:directory-files path)))
            (Ok (cl:mapcar #'cl:namestring entries)))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Directory list error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Directory list error: ~A" e)))))))

  (declare create-directory (String -> (Result FSError Unit)))
  (define (create-directory path)
    "Create directory and all parent directories as needed.

    Side effects:
    - Recursively creates all missing parent directories
    - No error if directory already exists
    - Equivalent to 'mkdir -p' behavior

    Returns (Ok Unit) if directory exists or was created successfully."
    (lisp (Result FSError Unit) (path)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist (cl:concatenate 'cl:string path "/"))
            (Ok Unit))
        (cl:file-error (e)
          (Err (IOError (cl:format cl:nil "Create directory error: ~A" e))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Create directory error: ~A" e)))))))

  (declare remove-directory (String -> (Result FSError Unit)))
  (define (remove-directory path)
    "Remove directory and ALL its contents recursively.

    WARNING: This is a DESTRUCTIVE operation that deletes:
    - All files in the directory
    - All subdirectories and their contents
    - The directory itself

    Equivalent to 'rm -rf' behavior. Use with extreme caution.
    Returns (Err (FileNotFound path)) if directory doesn't exist."
    (lisp (Result FSError Unit) (path)
      (cl:handler-case
          (cl:progn
            (uiop:delete-directory-tree (cl:pathname path) :validate cl:t)
            (Ok Unit))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Remove directory error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Remove directory error: ~A" e)))))))

  ;; Path utilities
  (declare join-paths (String -> String -> String))
  (define (join-paths path1 path2)
    "Join two path components"
    (lisp String (path1 path2)
      (cl:namestring (cl:merge-pathnames path2 path1))))

  (declare absolute-path (String -> (Result FSError String)))
  (define (absolute-path path)
    "Get absolute path"
    (lisp (Result FSError String) (path)
      (cl:handler-case
          (Ok (cl:namestring (cl:truename path)))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Absolute path error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Absolute path error: ~A" e)))))))

  (declare path-filename (String -> String))
  (define (path-filename path)
    "Get filename from path"
    (lisp String (path)
      (cl:file-namestring path)))

  (declare path-extension (String -> String))
  (define (path-extension path)
    "Get file extension"
    (lisp String (path)
      (cl:let ((ext (cl:pathname-type path)))
        (cl:if ext ext ""))))

  ;; Line-based operations
  (declare read-lines (String -> (Result FSError (List String))))
  (define (read-lines path)
    "Read file as list of lines"
    (lisp (Result FSError (List String)) (path)
      (cl:handler-case
          (cl:with-open-file (stream path :direction :input)
            (Ok (cl:loop for line = (cl:read-line stream cl:nil cl:nil)
                         while line
                         collect line)))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Read lines error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Read lines error: ~A" e)))))))

  (declare write-lines (String -> (List String) -> (Result FSError Unit)))
  (define (write-lines path lines)
    "Write list of lines to file"
    (lisp (Result FSError Unit) (path lines)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :if-exists :supersede)
              (cl:dolist (line lines)
                (cl:write-line line stream)))
            (Ok Unit))
        (cl:file-error (e)
          (Err (IOError (cl:format cl:nil "Write lines error: ~A" e))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Write lines error: ~A" e)))))))

  ;; Byte operations
  (declare read-file-bytes (String -> (Result FSError (List Integer))))
  (define (read-file-bytes path)
    "Read entire file contents as list of bytes.

    Performance: Uses efficient file-length and read-sequence for better
    performance with large files."
    (lisp (Result FSError (List Integer)) (path)
      (cl:handler-case
          (cl:with-open-file (stream path :direction :input :element-type '(cl:unsigned-byte 8))
            ;; Get file size for efficient reading
            (cl:let* ((file-length (cl:file-length stream))
                      (byte-vector (cl:make-array file-length :element-type '(cl:unsigned-byte 8))))
              (cl:read-sequence byte-vector stream)
              ;; Convert vector back to list for API compatibility
              (Ok (cl:coerce byte-vector 'cl:list))))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Read bytes error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Read bytes error: ~A" e)))))))

  (declare write-file-bytes (String -> (List Integer) -> (Result FSError Unit)))
  (define (write-file-bytes path bytes)
    "Write list of bytes to file (overwrites existing).

    Performance: Converts list to vector before writing for optimal performance
    with large byte arrays."
    (lisp (Result FSError Unit) (path bytes)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :element-type '(cl:unsigned-byte 8) :if-exists :supersede)
              ;; Convert list to vector for efficient writing
              (cl:let ((byte-vector (cl:make-array (cl:length bytes)
                                                   :element-type '(cl:unsigned-byte 8)
                                                   :initial-contents bytes)))
                (cl:write-sequence byte-vector stream)))
            (Ok Unit))
        (cl:file-error (e)
          (Err (IOError (cl:format cl:nil "Write bytes error: ~A" e))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Write bytes error: ~A" e)))))))

  ;; File operations
  (declare copy-file (String -> String -> (Result FSError Unit)))
  (define (copy-file source dest)
    "Copy file from source to destination"
    (lisp (Result FSError Unit) (source dest)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist dest)
            (uiop:copy-file source dest)
            (Ok Unit))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound source)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied (cl:format cl:nil "~A -> ~A" source dest))))
            (cl:t (Err (IOError (cl:format cl:nil "Copy error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Copy error: ~A" e)))))))

  (declare move-file (String -> String -> (Result FSError Unit)))
  (define (move-file source dest)
    "Move file from source to destination"
    (lisp (Result FSError Unit) (source dest)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist dest)
            (uiop:rename-file-overwriting-target source dest)
            (Ok Unit))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound source)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied (cl:format cl:nil "~A -> ~A" source dest))))
            (cl:t (Err (IOError (cl:format cl:nil "Move error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Move error: ~A" e)))))))

  (declare rename-file (String -> String -> (Result FSError Unit)))
  (define (rename-file old-path new-path)
    "Rename file (alias for move-file)"
    (move-file old-path new-path))

  ;; Simplified boolean queries
  (declare is-file? (String -> (Result FSError Boolean)))
  (define (is-file? path)
    "Check if path exists and is a regular file"
    (file-exists? path))

  (declare is-directory? (String -> (Result FSError Boolean)))
  (define (is-directory? path)
    "Check if path exists and is a directory"
    (directory-exists? path))

  ;; File size getter
  (declare file-size (String -> (Result FSError Integer)))
  (define (file-size path)
    "Get file size in bytes"
    (match (get-file-info path)
      ((Ok (FileInfo size _ _ _)) (Ok size))
      ((Err e) (Err e))))

  ;; Directory aliases
  (declare create-directories (String -> (Result FSError Unit)))
  (define (create-directories path)
    "Create directory and parents if needed (alias for create-directory)"
    (create-directory path))

  ;; Current directory
  (declare current-directory (Unit -> (Result FSError String)))
  (define (current-directory)
    "Get current working directory"
    (lisp (Result FSError String) ()
      (cl:handler-case
          (Ok (cl:namestring (uiop:getcwd)))
        (cl:file-error (e)
          (Err (IOError (cl:format cl:nil "Cannot get current directory: ~A" e))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Cannot get current directory: ~A" e)))))))

  ;; Temporary file utilities with proper Result handling
  (declare with-temp-file ((String -> (Result FSError String)) -> (Result FSError String)))
  (define (with-temp-file f)
    "Execute function with a temporary file path, with automatic cleanup.

    Creates a unique temporary file path in /tmp and passes it to the function.
    After the function completes (success or failure), attempts to delete the
    temporary file if it exists.

    Cleanup behavior:
    - Always attempts cleanup, even if function fails
    - Silently ignores cleanup errors to avoid masking function results
    - Temporary files have format: /tmp/smelter-temp-{timestamp}.tmp

    The function receives a file path string and should return a Result type."
    (let ((temp-path (lisp String ()
                       (cl:let ((temp-name (cl:format cl:nil "/tmp/smelter-temp-~A.tmp" (cl:get-universal-time))))
                         temp-name))))
      (let ((result (f temp-path)))
        (lisp Unit (temp-path)
          (cl:handler-case
              (cl:when (cl:probe-file temp-path)
                (cl:delete-file temp-path))
            (cl:error (e)
              ;; Log cleanup failure to stderr but don't propagate error
              ;; to avoid masking the function's result
              (cl:format cl:*error-output*
                         "Warning: Failed to cleanup temp file ~A: ~A~%"
                         temp-path e))))
        result)))

  (declare with-temp-directory ((String -> (Result FSError String)) -> (Result FSError String)))
  (define (with-temp-directory f)
    "Execute function with a temporary directory path, with automatic cleanup.

    Creates a unique temporary directory in /tmp and passes it to the function.
    After the function completes (success or failure), attempts to recursively
    delete the temporary directory and all its contents.

    Side effects:
    - Creates the temporary directory before calling the function
    - Recursively deletes directory and contents after function completes
    - Silently ignores cleanup errors to avoid masking function results

    Cleanup behavior:
    - Always attempts cleanup, even if function fails
    - Deletes ALL contents recursively (equivalent to 'rm -rf')
    - Temporary directories have format: /tmp/smelter-temp-dir-{timestamp}/

    The function receives a directory path string and should return a Result type."
    (let ((temp-dir (lisp String ()
                      (cl:let ((temp-name (cl:format cl:nil "/tmp/smelter-temp-dir-~A/" (cl:get-universal-time))))
                        (cl:ensure-directories-exist temp-name)
                        temp-name))))
      (let ((result (f temp-dir)))
        (lisp Unit (temp-dir)
          (cl:handler-case
              (cl:when (cl:probe-file temp-dir)
                (uiop:delete-directory-tree (cl:pathname temp-dir) :validate cl:t))
            (cl:error (e)
              ;; Log cleanup failure to stderr but don't propagate error
              ;; to avoid masking the function's result
              (cl:format cl:*error-output*
                         "Warning: Failed to cleanup temp directory ~A: ~A~%"
                         temp-dir e))))
        result))))