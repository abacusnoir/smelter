;;;; src/coalton-translator.lisp
;;;; Translates pure Coalton syntax to executable Common Lisp + Coalton hybrid

(defpackage :smelter.translator
  (:use :cl)
  (:export #:translate-pure-coalton
           #:wrap-for-execution
           #:extract-main-function
           #:parse-coalton-file))

(in-package :smelter.translator)

;;; Utility functions
(defun string-contains-p (substring string)
  "Check if string contains substring"
  (search substring string))

;;; Parse structure for pure Coalton files
(defstruct coalton-script
  "Represents a parsed pure Coalton script"
  (imports nil)
  (declarations nil)
  (definitions nil)
  (lisp-forms nil)        ; Common Lisp forms like defun
  (main-function nil)
  (has-main-p nil))

(defun read-all-forms (content)
  "Read all forms from a string containing Coalton code"
  (let ((*package* (find-package :cl-user)))
    (with-input-from-string (stream content)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            collect (preprocess-list-syntax form)))))

(defun preprocess-list-syntax (form)
  "Transform (list ...) syntax into Coalton-compatible cons chains"
  (labels ((make-coalton-list (elements)
             (if (null elements)
                 'coalton-user::nil
                 `(coalton-user::cons 
                   ,(preprocess-list-syntax (first elements))
                   ,(make-coalton-list (rest elements))))))
    (cond
      ;; Empty list: (list) -> nil
      ((equal form '(list)) 'coalton-user::nil)
      
      ;; List with elements: (list 1 2 3) -> (cons 1 (cons 2 (cons 3 nil)))
      ((and (listp form) (eq (first form) (intern "LIST" :cl-user)))
       (make-coalton-list (rest form)))
      
      ;; Process nested forms recursively
      ((listp form)
       (mapcar #'preprocess-list-syntax form))
      
      ;; Leave atoms unchanged
      (t form))))

(defun parse-coalton-file (content)
  "Parse pure Coalton content into structured forms"
  (let ((script (make-coalton-script))
        (forms (read-all-forms content)))
    
    (dolist (form forms)
      (cond
        ;; Handle coalton-toplevel forms - extract their contents
        ((and (listp form) (eq (first form) 'coalton-toplevel))
         (dolist (inner-form (rest form))
           (cond
             ;; Handle imports within coalton-toplevel
             ((and (listp inner-form) (eq (first inner-form) 'import))
              (push inner-form (coalton-script-imports script)))
             
             ;; Handle type declarations within coalton-toplevel
             ((and (listp inner-form) (eq (first inner-form) 'declare))
              (push inner-form (coalton-script-declarations script)))
             
             ;; Handle type definitions within coalton-toplevel  
             ((and (listp inner-form) (eq (first inner-form) 'define-type))
              (push inner-form (coalton-script-declarations script)))
             
             ;; Handle definitions within coalton-toplevel
             ((and (listp inner-form) (eq (first inner-form) 'define))
              (let ((def-name (if (listp (second inner-form))
                                 (first (second inner-form))
                                 (second inner-form))))
                (push inner-form (coalton-script-definitions script))
                ;; Check if this is a main function
                (when (eq def-name 'main)
                  (setf (coalton-script-has-main-p script) t)
                  (setf (coalton-script-main-function script) inner-form))))
             
             ;; Handle other forms within coalton-toplevel
             (t
              (push inner-form (coalton-script-definitions script))))))
        
        ;; Handle imports
        ((and (listp form) (eq (first form) 'import))
         (push form (coalton-script-imports script)))
        
        ;; Handle type declarations
        ((and (listp form) (eq (first form) 'declare))
         (push form (coalton-script-declarations script)))
        
        ;; Handle type definitions
        ((and (listp form) (eq (first form) 'define-type))
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
        
        ;; Handle Common Lisp forms (defun, defvar, etc.)
        ((and (listp form) (member (first form) '(defun defvar defparameter defconstant defmacro)))
         (push form (coalton-script-lisp-forms script))
         ;; Check if this is a main function in Common Lisp
         (when (and (eq (first form) 'defun) (eq (second form) 'main))
           (setf (coalton-script-has-main-p script) t)
           (setf (coalton-script-main-function script) form)))
        
        ;; Handle standalone expressions (for REPL/eval)
        (t
         (push form (coalton-script-definitions script)))))
    
    ;; Reverse to maintain order
    (setf (coalton-script-imports script) (nreverse (coalton-script-imports script)))
    (setf (coalton-script-declarations script) (nreverse (coalton-script-declarations script)))
    (setf (coalton-script-definitions script) (nreverse (coalton-script-definitions script)))
    (setf (coalton-script-lisp-forms script) (nreverse (coalton-script-lisp-forms script)))
    
    script))

(defun translate-for-repl (script)
  "Translate Coalton forms for REPL evaluation"
  (let ((forms (coalton-script-definitions script)))
    (with-output-to-string (out)
      ;; Set up coalton-user package which has proper Coalton environment
      (format out "(cl:progn~%")
      (format out "  (cl:in-package :coalton-user)~%")
      ;; Import smelter standard library functions
      (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.io :coalton-user))~%")
      (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.system :coalton-user))~%")
      
      (if (= (length forms) 1)
          ;; Single expression: evaluate directly in coalton context
          (format out "  (coalton:coalton ~S)" (first forms))
          ;; Multiple forms: wrap in toplevel
          (progn
            (format out "  (coalton:coalton-toplevel~%")
            (dolist (form forms)
              (format out "    ~S~%"
 form))
            (format out "  ))"))))))

(defun qualify-cl-symbols (form)
  "Recursively qualify Common Lisp symbols that might be shadowed by Coalton"
  (let ((cl-symbol-names '("PROGN" "DEFUN" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFMACRO"
                           "FORMAT" "LET" "LET*" "LAMBDA" "COND" "IF" "WHEN" "UNLESS"
                           "LIST" "CONS" "CAR" "CDR" "FIRST" "REST"
                           "AND" "OR" "NOT" "NULL" "EQ" "EQUAL"
                           "LOOP" "DOLIST" "DOTIMES"
                           "HANDLER-CASE" "ERROR" "IGNORE-ERRORS")))
    (cond
      ;; If it's a list starting with a CL symbol, qualify it
      ((and (listp form)
            (symbolp (first form))
            (member (symbol-name (first form)) cl-symbol-names :test #'string=))
       (cons (intern (symbol-name (first form)) :cl)
             (mapcar #'qualify-cl-symbols (rest form))))
      
      ;; Recursively process nested lists
      ((listp form)
       (mapcar #'qualify-cl-symbols form))
      
      ;; Individual symbols that should be qualified (less common case)
      ((and (symbolp form)
            (member (symbol-name form) cl-symbol-names :test #'string=))
       (intern (symbol-name form) :cl))
      
      ;; Leave everything else unchanged
      (t form))))

(defun translate-for-script (script)
  "Translate a script containing both Coalton and Common Lisp forms"
  (let ((coalton-forms (append
                        (coalton-script-imports script)
                        (coalton-script-declarations script)
                        (coalton-script-definitions script)))
        (lisp-forms (coalton-script-lisp-forms script)))
    
    (with-output-to-string (out)
      ;; Add package setup  
      (format out "(cl:progn~%")
      (format out "  (cl:in-package :coalton-user)~%~%")
      
      ;; Add smelter standard library imports
      (format out "  ;; Import Smelter standard libraries~%")
      (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.io :coalton-user))~%")
      (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.system :coalton-user))~%~%")
      
      ;; Only include coalton-toplevel if we have Coalton forms
      (when coalton-forms
        (format out "  (coalton:coalton-toplevel~%")
        ;; Add user's Coalton code
        (dolist (form coalton-forms)
          (format out "    ~S~%"
 form))
        (format out "  )~%~%"))
      
      ;; Add Common Lisp forms directly (outside coalton-toplevel) with qualification
      (dolist (form lisp-forms)
        (let ((qualified-form (qualify-cl-symbols form)))
          ;; Print qualified form with explicit package context to force cl: prefixes
          (let ((*package* (find-package :coalton-user)))
            (format out "  ~S~%~%" qualified-form))))
      
      ;; Auto-execute main function if it exists
      (when (coalton-script-has-main-p script)
        (format out "  ;; Auto-execute main function~%")
        (format out "  (cl:handler-case~%")
        (format out "    (main)~%")
        (format out "    (cl:error (e)~%")
        (format out "      (cl:format cl:*error-output* \"Error in main: ~~A~~%\" e)~%")
        (format out "      (uiop:quit 1)))~%"))
        
      (format out ")~%"))))

(defun translate-pure-coalton (content &key (for-repl nil))
  "Translate pure Coalton syntax to executable hybrid format"
  ;; Check if content is already wrapped in coalton-toplevel
  (if (and (not for-repl) (string-contains-p "coalton-toplevel" content))
      ;; Already has coalton-toplevel, just wrap in basic execution context
      (with-output-to-string (out)
        (format out "(cl:progn~%")
        (format out "  (cl:in-package :coalton-user)~%")
        (format out "  ;; Import Smelter standard libraries~%")
        (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.io :coalton-user))~%")
        (format out "  (cl:ignore-errors (cl:use-package :smelter.stdlib.system :coalton-user))~%")
        (format out "  ~A)" content))
      ;; Standard translation path
      (let ((script (parse-coalton-file content)))
        (if for-repl
            ;; REPL mode: evaluate expressions directly
            (translate-for-repl script)
            ;; Script mode: create full program
            (translate-for-script script)))))

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