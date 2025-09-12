;;;; src/stdlib/smelter-io.lisp
;;;; Smelter I/O primitives in pure Coalton

(defpackage :smelter.stdlib.io
  (:use :coalton :coalton-prelude)
  (:export
   ;; Basic I/O
   #:io-print
   #:io-println))

(in-package :smelter.stdlib.io)

(coalton:coalton-toplevel
  
  ;;; Basic I/O Operations
  
  (declare io-print (String -> Unit))
  (define (io-print str)
    "Print a string to standard output"
    (lisp Unit (str)
      (cl:progn
        (cl:write-string str)
        (cl:force-output)
        Unit)))
  
  (declare io-println (String -> Unit))
  (define (io-println str)
    "Print a string with newline"
    (lisp Unit (str)
      (cl:progn
        (cl:write-line str)
        (cl:force-output)
        Unit))))