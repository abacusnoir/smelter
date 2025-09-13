;;;; src/coalton-translator.lisp
;;;; Translates pure Coalton syntax to executable Common Lisp + Coalton hybrid

(defpackage :smelter.translator
  (:use :cl)
  (:export #:translate-pure-coalton
           #:wrap-for-execution
           #:extract-main-function
           #:parse-coalton-file))

(in-package :smelter.translator)

;;; Parse structure for pure Coalton files
(defstruct coalton-script
  "Represents a parsed pure Coalton script"
  (imports nil)
  (declarations nil)
  (definitions nil)
  (main-function nil)
  (has-main-p nil))

(defun read-all-forms (content)
  "Read all forms from a string containing Coalton code"
  (with-input-from-string (stream content)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun parse-coalton-file (content)
  "Parse pure Coalton content into structured forms"
  (let ((script (make-coalton-script))
        (forms (read-all-forms content)))
    
    (dolist (form forms)
      (cond
        ;; Handle imports
        ((and (listp form) (eq (first form) 'import))
         (push form (coalton-script-imports script)))
        
        ;; Handle type declarations
        ((and (listp form) (eq (first form) 'declare))
         (push form (coalton-script-declarations script)))
        
        ;; Handle definitions
        ((and (listp form) (eq (first form) 'define))
         (let ((def-name (if (listp (second form))
                            (first (second form))
                            (second form))))
           (push form (coalton-script-definitions script))
           ;; Check if this is a main function
           (when (eq def-name 'main)
             (setf (coalton-script-has-main-p script) t)
             (setf (coalton-script-main-function script) form))))
        
        ;; Handle standalone expressions (for REPL/eval)
        (t
         (push form (coalton-script-definitions script)))))
    
    ;; Reverse to maintain order
    (setf (coalton-script-imports script) (nreverse (coalton-script-imports script)))
    (setf (coalton-script-declarations script) (nreverse (coalton-script-declarations script)))
    (setf (coalton-script-definitions script) (nreverse (coalton-script-definitions script)))
    
    script))


(defun translate-for-repl-simple (script)
  "Simplified REPL translation using coalton-toplevel"
  (let ((forms (coalton-script-definitions script)))
    (with-output-to-string (out)
      (format out "(progn~%")
      (format out "  (in-package :smelter.user)~%")
      ;; Import Smelter standard library
      (format out "  (ignore-errors (use-package :smelter.stdlib.prelude :smelter.user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.io :smelter.user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.system :smelter.user))~%")
      
      ;; For single expressions, use coalton:coalton
      ;; For multiple forms or definitions, use coalton-toplevel
      (if (and (= (length forms) 1)
               (not (and (listp (first forms))
                        (eq (first (first forms)) 'define))))
          ;; Single expression - use coalton
          (format out "  (coalton:coalton ~S))" (first forms))
          ;; Multiple forms or definitions - use toplevel
          (progn
            (format out "  (coalton:coalton-toplevel~%")
            (dolist (form forms)
              (format out "    ~S~%" form))
            (format out "  ))"))))))

(defun translate-for-script-simple (script)
  "Simplified script translation using coalton-toplevel"
  (let ((coalton-forms (append
                        (coalton-script-imports script)
                        (coalton-script-declarations script)
                        (coalton-script-definitions script))))
    
    (with-output-to-string (out)
      (format out "(progn~%")
      (format out "  (in-package :smelter.user)~%")
      ;; Import Smelter standard library
      (format out "  (ignore-errors (use-package :smelter.stdlib.prelude :smelter.user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.io :smelter.user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.system :smelter.user))~%")
      
      ;; Wrap ALL Coalton code in coalton-toplevel
      (format out "  (coalton:coalton-toplevel~%")
      (dolist (form coalton-forms)
        (format out "    ~S~%" form))
      (format out "  )~%")
      
      ;; If main exists, call it
      (when (coalton-script-has-main-p script)
        (format out "  (coalton:coalton (main))~%"))
        
      (format out ")~%"))))

(defun translate-pure-coalton (content &key (for-repl nil))
  "Translate pure Coalton syntax by wrapping in coalton-toplevel"
  (let ((script (parse-coalton-file content)))
    
    (if for-repl
        ;; REPL mode: evaluate expressions directly
        (translate-for-repl-simple script)
        ;; Script mode: create full program
        (translate-for-script-simple script))))

(defun wrap-for-execution (coalton-code)
  "Wrap translated Coalton code for safe execution"
  (with-output-to-string (out)
    (format out "(handler-case~%")
    (format out "  (progn ~A)~%" coalton-code)
    (format out "  (error (e)~%")
    (format out "    (format *error-output* \"Error: ~~A~~%\" e)~%")
    (format out "    (error e)))")))

;;; Helper functions for main detection and extraction

(defun extract-main-signature (main-form)
  "Extract the signature of the main function"
  (when (and (listp main-form)
             (eq (first main-form) 'define))
    (let ((signature (second main-form)))
      (cond
        ;; (define main ...) - no parameters
        ((atom signature) :no-params)
        ;; (define (main) ...) - no parameters
        ((null (rest signature)) :no-params)
        ;; (define (main args) ...) - with parameters
        ((= (length signature) 2) :with-args)
        (t :unknown)))))

(defun generate-main-wrapper (main-signature)
  "Generate appropriate main wrapper based on signature"
  (case main-signature
    (:no-params
     "(defun cl-main ()
        (coalton:coalton (main)))")
    (:with-args
     "(defun cl-main ()
        (let ((args (uiop:command-line-arguments)))
          (coalton:coalton (main args))))")
    (t
     "(defun cl-main ()
        (error \"Invalid main function signature\"))")))

;;; Error position tracking

(defstruct source-map
  "Maps translated code positions back to original source"
  (original-lines nil)
  (translated-lines nil)
  (line-mapping nil))

(defun create-source-map (original translated)
  "Create mapping between original and translated source"
  (make-source-map
   :original-lines (split-string original #\Newline)
   :translated-lines (split-string translated #\Newline)
   :line-mapping (compute-line-mapping original translated)))

(defun compute-line-mapping (original translated)
  "Compute line number mappings"
  ;; Simplified: assumes roughly 1:1 mapping with offset
  ;; In production, would need proper source tracking
  (let ((offset 5)) ; Account for boilerplate
    (lambda (translated-line)
      (max 1 (- translated-line offset)))))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start end)
        while end))

(defun extract-main-function (script)
  "Extract main function from parsed script"
  (coalton-script-main-function script))