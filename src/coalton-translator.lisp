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

(defun translate-for-repl (script)
  "Translate Coalton forms for REPL evaluation"
  (let ((forms (coalton-script-definitions script)))
    (with-output-to-string (out)
      ;; Set up coalton-user package which has proper Coalton environment
      (format out "(progn~%")
      (format out "  (in-package :coalton-user)~%")
      ;; Import smelter standard library functions
      (format out "  (ignore-errors (use-package :smelter.stdlib.io :coalton-user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.system :coalton-user))~%")
      
      (if (= (length forms) 1)
          ;; Single expression: evaluate directly in coalton context
          (format out "  (coalton:coalton ~S))" (first forms))
          ;; Multiple forms: wrap in toplevel
          (progn
            (format out "  (coalton:coalton-toplevel~%")
            (dolist (form forms)
              (format out "    ~S~%" form))
            (format out "  ))"))))))

(defun translate-for-script (script)
  "Translate a Coalton script for execution"
  (let ((coalton-forms (append
                        (coalton-script-imports script)
                        (coalton-script-declarations script)
                        (coalton-script-definitions script))))
    
    (with-output-to-string (out)
      ;; Add package setup  
      (format out "(progn~%")
      (format out "  (in-package :coalton-user)~%~%")
      
      ;; Add smelter standard library imports
      (format out "  ;; Import Smelter standard libraries~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.io :coalton-user))~%")
      (format out "  (ignore-errors (use-package :smelter.stdlib.system :coalton-user))~%~%")
      (format out "  (coalton:coalton-toplevel~%")
      
      ;; Add user's Coalton code
      (dolist (form coalton-forms)
        (format out "    ~S~%" form))
      (format out "  )~%")
      
      ;; Generate main wrapper if main exists
      (when (coalton-script-has-main-p script)
        (format out "  (defun cl-main ()~%")
        (format out "    \"Auto-generated main wrapper\"~%")
        (format out "    (handler-case~%")
        (format out "      (progn~%")
        (format out "        (coalton:coalton (main))~%")
        (format out "        (fresh-line)~%")
        (format out "        (force-output))~%")
        (format out "      (error (e)~%")
        (format out "        (format *error-output* \"Error: ~~A~~%\" e)~%")
        (format out "        (uiop:quit 1))))~%~%")
        
        ;; Set as entry point
        (format out "  (setf smelter.cli:*script-main* #'cl-main)~%"))
        
      (format out ")~%"))))

(defun translate-pure-coalton (content &key (for-repl nil))
  "Translate pure Coalton syntax to executable hybrid format"
  (let ((script (parse-coalton-file content)))
    
    (if for-repl
        ;; REPL mode: evaluate expressions directly
        (translate-for-repl script)
        ;; Script mode: create full program
        (translate-for-script script))))

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