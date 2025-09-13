(defpackage #:smelter/adapters/fs
  (:use #:coalton #:coalton-prelude)
  (:export
   #:FileInfo
   #:FSError #:FileNotFound #:PermissionDenied #:IOError
   #:read-file
   #:write-file
   #:append-file
   #:delete-file
   #:file-exists?
   #:directory-exists?
   #:get-file-info
   #:list-directory
   #:create-directory
   #:remove-directory
   #:join-paths
   #:absolute-path
   #:path-filename
   #:path-extension
   #:read-lines
   #:write-lines))

(in-package #:smelter/adapters/fs)

(coalton-toplevel
  (define-type FileInfo
    (FileInfo Integer Integer Boolean String))

  (define-type FSError
    (FileNotFound String)
    (PermissionDenied String)  
    (IOError String))

  (declare handle-file-error (cl:condition -> FSError))
  (define (handle-file-error condition)
    "Convert Common Lisp file condition to FSError"
    (lisp FSError (condition)
      (cl:cond 
        ((cl:typep condition 'cl:file-does-not-exist)
         (cl:make-instance 'FileNotFound 
                           :string (cl:format cl:nil "File not found: ~A" condition)))
        ((cl:typep condition 'cl:file-error)  
         (cl:make-instance 'IOError
                           :string (cl:format cl:nil "File I/O error: ~A" condition)))
        (cl:t
         (cl:make-instance 'IOError 
                           :string (cl:format cl:nil "Filesystem error: ~A" condition))))))

  (declare read-file (String -> (Result String FSError)))
  (define (read-file filepath)
    "Read entire file contents as string"
    (lisp (Result String FSError) (filepath)
      (cl:handler-case
          (cl:with-open-file (stream filepath :direction :input :external-format :utf-8)
            (cl:let ((content (cl:make-string (cl:file-length stream))))
              (cl:read-sequence content stream)
              (cl:values 
               (cl:make-instance 'coalton:Ok :ok content)
               cl:t)))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare write-file (String -> String -> (Result Unit FSError)))
  (define (write-file filepath content)
    "Write string content to file (overwrites existing)"
    (lisp (Result Unit FSError) (filepath content)
      (cl:handler-case
          (cl:with-open-file (stream filepath :direction :output 
                                     :if-exists :supersede 
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
            (cl:write-string content stream)
            (cl:values
             (cl:make-instance 'coalton:Ok :ok coalton:Unit)
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare append-file (String -> String -> (Result Unit FSError)))
  (define (append-file filepath content)
    "Append string content to file"
    (lisp (Result Unit FSError) (filepath content)
      (cl:handler-case
          (cl:with-open-file (stream filepath :direction :output 
                                     :if-exists :append
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
            (cl:write-string content stream)
            (cl:values
             (cl:make-instance 'coalton:Ok :ok coalton:Unit)
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare delete-file (String -> (Result Unit FSError)))
  (define (delete-file filepath)
    "Delete file from filesystem"
    (lisp (Result Unit FSError) (filepath)
      (cl:handler-case
          (cl:progn
            (cl:delete-file filepath)
            (cl:values
             (cl:make-instance 'coalton:Ok :ok coalton:Unit)
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare file-exists? (String -> Boolean))
  (define (file-exists? filepath)
    "Check if file exists"
    (lisp Boolean (filepath)
      (cl:and (cl:probe-file filepath) 
              (cl:not (cl:directory-pathname-p filepath)))))

  (declare directory-exists? (String -> Boolean))
  (define (directory-exists? dirpath)
    "Check if directory exists"
    (lisp Boolean (dirpath)
      (cl:and (cl:probe-file dirpath)
              (cl:directory-pathname-p dirpath))))

  (declare get-file-info (String -> (Result FileInfo FSError)))
  (define (get-file-info filepath)
    "Get file metadata information"
    (lisp (Result FileInfo FSError) (filepath)
      (cl:handler-case
          (cl:let* ((path (cl:probe-file filepath))
                    (truename (cl:truename path)))
            (cl:multiple-value-bind (second minute hour day month year)
                (cl:decode-universal-time (cl:file-write-date truename))
              (cl:declare (cl:ignore second minute hour))
              (cl:let ((size (cl:with-open-file (stream truename)
                               (cl:file-length stream)))
                       (modified-time (+ (* (- year 1900) 365 24 60 60)
                                       (* (- month 1) 30 24 60 60)  
                                       (* (- day 1) 24 60 60)))
                       (is-dir (cl:directory-pathname-p truename))
                       (name (cl:file-namestring truename)))
                (cl:values
                 (cl:make-instance 'coalton:Ok 
                                   :ok (cl:make-instance 'FileInfo
                                                         :integer size
                                                         :integer modified-time
                                                         :boolean is-dir
                                                         :string name))
                 cl:t))))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare list-directory (String -> (Result (List String) FSError)))
  (define (list-directory dirpath)
    "List contents of directory"
    (lisp (Result (List String) FSError) (dirpath)
      (cl:handler-case
          (cl:let ((entries (cl:directory (cl:merge-pathnames "*" dirpath))))
            (cl:values
             (cl:make-instance 'coalton:Ok
                               :ok (cl:mapcar #'cl:namestring entries))
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare create-directory (String -> (Result Unit FSError)))
  (define (create-directory dirpath)
    "Create directory (and parent directories if needed)"
    (lisp (Result Unit FSError) (dirpath)
      (cl:handler-case
          (cl:progn
            (cl:ensure-directories-exist dirpath)
            (cl:values
             (cl:make-instance 'coalton:Ok :ok coalton:Unit)
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare remove-directory (String -> (Result Unit FSError)))
  (define (remove-directory dirpath)
    "Remove empty directory"
    (lisp (Result Unit FSError) (dirpath)
      (cl:handler-case
          (cl:progn
            #+sbcl (sb-posix:rmdir dirpath)
            #-sbcl (cl:error "Directory removal not implemented for this Lisp")
            (cl:values
             (cl:make-instance 'coalton:Ok :ok coalton:Unit)
             cl:t))
        (cl:condition (e)
          (cl:values
           (cl:make-instance 'coalton:Err :err (handle-file-error e))
           cl:t)))))

  (declare join-paths (String -> String -> String))
  (define (join-paths base-path relative-path)
    "Join two filesystem paths"
    (lisp String (base-path relative-path)
      (cl:namestring 
       (cl:merge-pathnames relative-path base-path))))

  (declare absolute-path (String -> String))
  (define (absolute-path filepath)
    "Get absolute path for given path"
    (lisp String (filepath)
      (cl:namestring (cl:truename filepath))))

  (declare path-filename (String -> String))
  (define (path-filename filepath)
    "Get filename part of path"
    (lisp String (filepath)
      (cl:file-namestring filepath)))

  (declare path-extension (String -> (Optional String)))
  (define (path-extension filepath)
    "Get file extension (without dot)"
    (lisp (Optional String) (filepath)
      (cl:let ((type (cl:pathname-type filepath)))
        (cl:if type
               (cl:make-instance 'coalton:Some :some type)
               (cl:make-instance 'coalton:None)))))

  (declare read-lines (String -> (Result (List String) FSError)))
  (define (read-lines filepath)
    "Read file as list of lines"
    (match (read-file filepath)
      ((Ok content) (Ok (string-lines content)))
      ((Err e) (Err e))))

  (declare write-lines (String -> (List String) -> (Result Unit FSError)))
  (define (write-lines filepath lines)
    "Write list of strings as lines to file"
    (let ((content (join-lines lines)))
      (write-file filepath content)))

  (declare string-lines (String -> (List String)))
  (define (string-lines s)
    "Split string into lines"
    (lisp (List String) (s)
      (split-sequence:split-sequence #\Newline s)))

  (declare join-lines ((List String) -> String))
  (define (join-lines lines)
    "Join list of strings with newlines"
    (lisp String (lines)
      (cl:format cl:nil "~{~A~^~%~}" lines))))