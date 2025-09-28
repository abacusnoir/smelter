(defpackage #:smelter/adapters/fs
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
    "Read entire file contents as string"
    (lisp (Result FSError String) (path)
      (cl:handler-case
          (cl:with-open-file (stream path :direction :input :if-does-not-exist :error)
            (Ok (cl:with-output-to-string (s)
                  (cl:loop for line = (cl:read-line stream cl:nil cl:nil)
                           while line
                           do (cl:write-line line s)))))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "File error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Read error: ~A" e)))))))

  (declare write-file (String -> String -> (Result FSError Unit)))
  (define (write-file path content)
    "Write string content to file (overwrites existing)"
    (lisp (Result FSError Unit) (path content)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :if-exists :supersede)
              (cl:write-string content stream))
            (Ok Unit))
        (cl:file-error (e)
          (cl:cond
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Write error: ~A" e))))))
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
          (cl:cond
            ((cl:search "does not exist" (cl:format cl:nil "~A" e))
             (Err (FileNotFound path)))
            ((cl:search "permission" (cl:format cl:nil "~A" e))
             (Err (PermissionDenied path)))
            (cl:t (Err (IOError (cl:format cl:nil "Delete error: ~A" e))))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Delete error: ~A" e)))))))

  ;; File/directory existence checks
  (declare file-exists? (String -> Boolean))
  (define (file-exists? path)
    "Check if file exists"
    (lisp Boolean (path)
      (cl:and (cl:probe-file path)
              (cl:not (uiop:directory-pathname-p path)))))

  (declare directory-exists? (String -> Boolean))
  (define (directory-exists? path)
    "Check if directory exists"
    (lisp Boolean (path)
      (cl:and (cl:probe-file path)
              (uiop:directory-pathname-p path))))

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
          (Err (FileNotFound path)))
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
    "Create directory (and parents if needed)"
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
    "Remove directory and its contents"
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

  (declare absolute-path (String -> String))
  (define (absolute-path path)
    "Get absolute path"
    (lisp String (path)
      (cl:namestring (cl:truename path))))

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
    "Read entire file contents as list of bytes"
    (lisp (Result FSError (List Integer)) (path)
      (cl:handler-case
          (cl:with-open-file (stream path :direction :input :element-type '(cl:unsigned-byte 8))
            (cl:let ((bytes cl:nil))
              (cl:loop for byte = (cl:read-byte stream cl:nil cl:nil)
                       while byte
                       do (cl:push byte bytes))
              (Ok (cl:nreverse bytes))))
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
    "Write list of bytes to file (overwrites existing)"
    (lisp (Result FSError Unit) (path bytes)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist path)
            (cl:with-open-file (stream path :direction :output :element-type '(cl:unsigned-byte 8) :if-exists :supersede)
              (cl:dolist (byte bytes)
                (cl:write-byte byte stream)))
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
  (declare is-file? (String -> Boolean))
  (define (is-file? path)
    "Check if path exists and is a regular file"
    (file-exists? path))

  (declare is-directory? (String -> Boolean))
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
  (declare current-directory (Unit -> String))
  (define (current-directory)
    "Get current working directory"
    (lisp String ()
      (cl:namestring (uiop:getcwd))))

  ;; Temporary file utilities (simplified for Boolean return type)
  (declare with-temp-file ((String -> Boolean) -> Boolean))
  (define (with-temp-file f)
    "Execute function with a temporary file path"
    (let ((temp-path (lisp String ()
                       (cl:let ((temp-name (cl:format cl:nil "/tmp/smelter-temp-~A.tmp" (cl:get-universal-time))))
                         temp-name))))
      (let ((result (f temp-path)))
        (lisp Unit (temp-path)
          (cl:handler-case
              (cl:delete-file temp-path)
            (cl:error ())))
        result)))

  (declare with-temp-directory ((String -> Boolean) -> Boolean))
  (define (with-temp-directory f)
    "Execute function with a temporary directory path"
    (let ((temp-dir (lisp String ()
                      (cl:let ((temp-name (cl:format cl:nil "/tmp/smelter-temp-dir-~A/" (cl:get-universal-time))))
                        temp-name))))
      (let ((result (f temp-dir)))
        (lisp Unit (temp-dir)
          (cl:handler-case
              (uiop:delete-directory-tree (cl:pathname temp-dir) :validate cl:t)
            (cl:error ())))
        result))))