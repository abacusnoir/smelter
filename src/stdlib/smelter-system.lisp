;;;; src/stdlib/smelter-system.lisp
;;;; System utilities for Smelter

(defpackage :smelter.stdlib.system
  (:use :coalton :coalton-prelude)
  (:export
   ;; Time operations
   #:current-time-millis
   #:sleep))

(in-package :smelter.stdlib.system)

(coalton:coalton-toplevel
  
  ;;; Time Operations
  
  (declare current-time-millis (Unit -> Integer))
  (define (current-time-millis _)
    "Get current time in milliseconds"
    (lisp Integer ()
      (cl:* 1000 (cl:get-universal-time))))
  
  (declare sleep (Integer -> Unit))
  (define (sleep millis)
    "Sleep for specified milliseconds"
    (lisp Unit (millis)
      (cl:progn
        (cl:sleep (cl:/ millis 1000.0))
        Unit))))