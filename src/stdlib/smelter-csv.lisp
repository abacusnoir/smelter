(cl:defpackage #:smelter.stdlib.csv
  (:use #:coalton #:coalton-prelude)
  (:export
   #:CSV
   #:ParseError #:IOError #:MalformedRow
   #:parse-csv
   #:read-csv
   #:stringify-csv
   #:write-csv))

(cl:in-package #:smelter.stdlib.csv)

(coalton-toplevel
  ;; Define CSV as a single-constructor type to access List and String properly
  (define-type CSV
    (CSV (List (List String))))

  (define-type ParseError
    "An error that can occur during CSV parsing."
    (IOError String)
    (MalformedRow String))

  (define (parse-csv content)
    "Parse a string containing CSV data into a CSV structure.

    Handles quoted fields and newlines. Returns a ParseError if the
    CSV is malformed."
    (lisp (Result ParseError CSV) (content)
      (cl:handler-case
          (cl:let* ((input (cl:make-string-input-stream content))
                    (rows (cl-csv:read-csv input)))
            (Ok (CSV rows)))
        (cl:error (e)
          (Err (MalformedRow (cl:format cl:nil "Failed to parse CSV: ~A" e)))))))

  (define (stringify-csv csv-data)
    "Convert a CSV data structure back into a valid CSV formatted string."
    (match csv-data
      ((CSV rows)
       (lisp String (rows)
         (cl:let ((output (cl:make-string-output-stream)))
           (cl-csv:write-csv rows :stream output)
           (cl:get-output-stream-string output))))))

  (define (read-csv filename)
    "Read a CSV file and return its contents as a CSV structure."
    (lisp (Result ParseError CSV) (filename)
      (cl:handler-case
          (cl:with-open-file (stream filename :direction :input)
            (Ok (CSV (cl-csv:read-csv stream))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Failed to read CSV file ~A: ~A" filename e)))))))

  (define (write-csv filename csv-data)
    "Write CSV data to a file."
    (lisp (Result ParseError Unit) (filename csv-data)
      (cl:handler-case
          (cl:progn
            (cl:with-open-file (stream filename :direction :output :if-exists :supersede)
              (cl-csv:write-csv csv-data :stream stream))
            (Ok Unit))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "Failed to write CSV file ~A: ~A" filename e))))))))