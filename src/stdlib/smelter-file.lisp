;;;; src/stdlib/smelter-file.lisp  
;;;; File I/O operations for Smelter using official Coalton patterns

(defpackage #:smelter.stdlib.file
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; File reading
   #:read-file
   #:file-exists-p
   ;; File writing  
   #:write-file
   #:append-file
   ;; Error types
   #:FileError
   #:FileNotFound #:PermissionDenied #:IOError))

(in-package #:smelter.stdlib.file)

(coalton-toplevel
  ;; Error type for file operations
  (define-type FileError
    (FileNotFound String)
    (PermissionDenied String)  
    (IOError String))
    
  ;;; File Reading Operations
  
  (declare read-file (String -> (Result FileError String)))
  (define (read-file path)
    "Read entire contents of a file as a string"
    (lisp (Result FileError String) (path)
      (cl:handler-case
          (Ok (alexandria:read-file-into-string path))
        (cl:file-error (e)
          (Err (FileNotFound (cl:format cl:nil "File not found: ~A" path))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "IO error: ~A" e)))))))
  
  (declare file-exists-p (String -> Boolean))
  (define (file-exists-p path)
    "Check if a file exists"
    (lisp Boolean (path)
      (cl:not (cl:null (cl:probe-file path)))))
      
  ;;; File Writing Operations
  
  (declare write-file (String -> String -> (Result FileError Unit)))
  (define (write-file path content)
    "Write string content to a file, overwriting if it exists"
    (lisp (Result FileError Unit) (path content)
      (cl:handler-case
          (cl:progn
            (cl:with-open-file (stream path 
                                       :direction :output 
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
              (cl:write-string content stream))
            (Ok Unit))
        (cl:file-error (e)
          (Err (PermissionDenied (cl:format cl:nil "Permission denied: ~A" path))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "IO error: ~A" e)))))))
  
  (declare append-file (String -> String -> (Result FileError Unit)))
  (define (append-file path content)
    "Append string content to a file"
    (lisp (Result FileError Unit) (path content)
      (cl:handler-case
          (cl:progn
            (cl:with-open-file (stream path 
                                       :direction :output 
                                       :if-exists :append
                                       :if-does-not-exist :create)
              (cl:write-string content stream))
            (Ok Unit))
        (cl:file-error (e)
          (Err (PermissionDenied (cl:format cl:nil "Permission denied: ~A" path))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "IO error: ~A" e))))))))