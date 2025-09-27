;;;
;;; build/create-image.lisp
;;;
;;; SBCL core creation for Smelter with Coalton support
;;;

;; Load essential systems
(format t "~&=== Starting Smelter Build ===~%")
(load "~/quicklisp/setup.lisp")
(require 'asdf)

;; Load the Smelter system definition from the project root
(asdf:load-asd (merge-pathnames "../smelter.asd" *load-truename*))

;; Ensure Coalton is loaded first with its full system
(format t "~&Loading Coalton system...~%")
(ql:quickload :coalton :silent nil)

;; Verify Coalton loaded properly
(unless (find-package :coalton)
  (error "Failed to load Coalton package"))

;; Ensure coalton-user package exists (it should be created by Coalton)

;; Load the entire Smelter system and its dependencies via Quicklisp/ASDF
(format t "~&Loading Smelter system...~%")
(ql:quickload :smelter :silent nil)

;; Return to cl-user package for saving
(in-package #:cl-user)

;; Save the final executable
(format t "~&=== Saving Smelter Executable ===~%")
(format t "~&Saving final executable 'smt'...~%")

(sb-ext:save-lisp-and-die "smt"
  :toplevel #'smelter:main
  :executable t
  :compression t      ; Enable compression for smaller binary
  :purify nil         ; CRITICAL: Preserve dynamic compilation info
  :save-runtime-options t) ; Preserve runtime options

;; This should not be reached, but just in case
(format t "~&✅ Build completed successfully!~%")
(uiop:quit 0)