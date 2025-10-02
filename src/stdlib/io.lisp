;;;; src/stdlib/io.lisp
;;;; Core I/O operations for Smelter

(defpackage #:smelter.stdlib.io
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; Console I/O
   #:io-print
   #:io-println
   #:io-read-line
   ;; Show functions for output
   #:show-int
   #:show-bool
   ;; Error handling
   #:IOError))

(in-package #:smelter.stdlib.io)

(coalton:coalton-toplevel

  ;; Error type for I/O operations
  (define-type IOError
    "I/O operation error"
    (IOError String))

  ;;; Console Output

  (declare io-print (String -> Unit))
  (define (io-print str)
    "Print a string to stdout without newline"
    (lisp Unit (str)
      (cl:princ str)
      (cl:finish-output)
      Unit))

  (declare io-println (String -> Unit))
  (define (io-println str)
    "Print a string to stdout with newline"
    (lisp Unit (str)
      (cl:format cl:t "~A~%" str)
      (cl:finish-output)
      Unit))

  ;;; Console Input

  (declare io-read-line (Unit -> (Optional String)))
  (define (io-read-line _)
    "Read a line from stdin, returns None on EOF"
    (lisp (Optional String) ()
      (cl:let ((line (cl:read-line cl:*standard-input* cl:nil cl:nil)))
        (cl:if line
               (Some line)
               None))))

  ;;; Show Functions - Convert values to strings

  (declare show-int (Integer -> String))
  (define (show-int n)
    "Convert an integer to a string"
    (lisp String (n)
      (cl:format cl:nil "~D" n)))

  (declare show-bool (Boolean -> String))
  (define (show-bool b)
    "Convert a boolean to a string"
    (if b "True" "False")))
