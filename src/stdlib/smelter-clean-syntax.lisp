;;;; src/stdlib/smelter-clean-syntax.lisp
;;;; User-friendly aliases for clean Coalton syntax

(defpackage #:smelter.stdlib.clean
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:io #:smelter.stdlib.io))
  (:export
   ;; Friendly I/O names
   #:println
   ;; #:print  ; Conflicts with CL:PRINT
   ;; #:read-line  ; Commented out for now
   ;; Friendly show functions
   #:show))

(in-package #:smelter.stdlib.clean)

(coalton:coalton-toplevel

  ;;; User-friendly I/O functions

  (declare println (String -> Unit))
  (define println io:io-println)

  ;; Note: 'print' conflicts with CL:PRINT, so it's not exported
  ;; Users can use io-print directly if needed

  ;; Note: read-line returns Optional String to handle EOF
  ;; For now, we don't wrap it - users should handle Optional
  ;; (declare read-line (Unit -> (Optional String)))
  ;; (define (read-line _)
  ;;   (io:io-read-line))

  ;;; Polymorphic show function using typeclass

  ;; For now, just handle the most common types
  (declare show (Integer -> String))
  (define (show n)
    (io:show-int n)))
