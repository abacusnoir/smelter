;;;; Smelter Prelude - Basic operators for pure Coalton code
;;;; These are exposed as Coalton functions that can be used in pure Coalton code

(defpackage :smelter.stdlib.prelude
  (:use :cl)
  (:export
   ;; Arithmetic
   #:smt-add
   #:smt-sub  
   #:smt-mul
   #:smt-div
   ;; Comparison
   #:smt-gt
   #:smt-lt
   #:smt-gte
   #:smt-lte
   #:smt-eq
   #:smt-neq
   ;; String
   #:smt-concat
   #:smt-show))

(in-package :smelter.stdlib.prelude)

;; Define these as Coalton functions
;; TEMPORARILY COMMENTED OUT - issues with coalton-toplevel syntax
#|
(coalton:coalton-toplevel
  
  ;; Arithmetic operators
  (define (smt-add x y)
    (lisp Integer (x y)
      (cl:+ x y)))
  
  (define (smt-sub x y)
    (lisp Integer (x y)
      (cl:- x y)))
  
  (define (smt-mul x y)
    (lisp Integer (x y)
      (cl:* x y)))
  
  (define (smt-div x y)
    (lisp Integer (x y)
      (cl:floor x y)))
  
  ;; Comparison operators
  (define (smt-gt x y)
    (lisp Boolean (x y)
      (if (cl:> x y) True False)))
  
  (define (smt-lt x y)
    (lisp Boolean (x y)
      (if (cl:< x y) True False)))
  
  (define (smt-gte x y)
    (lisp Boolean (x y)
      (if (cl:>= x y) True False)))
  
  (define (smt-lte x y)
    (lisp Boolean (x y)
      (if (cl:<= x y) True False)))
  
  (define (smt-eq x y)
    (lisp Boolean (x y)
      (if (cl:= x y) True False)))
  
  (define (smt-neq x y)
    (lisp Boolean (x y)
      (if (cl:/= x y) True False)))
  
  ;; String operations
  (define (smt-concat s1 s2)
    (lisp String (s1 s2)
      (cl:concatenate 'cl:string s1 s2)))
  
  (define (smt-show n)
    (lisp String (n)
      (cl:format cl:nil "~D" n))))
|#

;; For now, just define these as simple Common Lisp functions
;; that will be available through the package
(defun smt-add (x y) (+ x y))
(defun smt-sub (x y) (- x y))
(defun smt-mul (x y) (* x y))
(defun smt-div (x y) (floor x y))
(defun smt-gt (x y) (> x y))
(defun smt-lt (x y) (< x y))
(defun smt-gte (x y) (>= x y))
(defun smt-lte (x y) (<= x y))
(defun smt-eq (x y) (= x y))
(defun smt-neq (x y) (/= x y))
(defun smt-concat (s1 s2) (concatenate 'string s1 s2))
(defun smt-show (n) (format nil "~D" n))

;; Now define aliases that users can import as the normal operators
;; This is done in the translator by defining these as macros in the user package

(provide 'smelter-prelude)