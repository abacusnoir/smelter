;;;; Coalton Preprocessor - Transform operators to fully qualified names
;;;; This allows users to write (+ 1 2) and have it transformed to (coalton-library/classes:+ 1 2)

(defpackage :smelter.preprocessor
  (:use :cl)
  (:export #:preprocess-coalton))

(in-package :smelter.preprocessor)

(defparameter *operator-mappings*
  '((+ . "coalton-library/classes:+")
    (- . "coalton-library/classes:-")
    (* . "coalton-library/classes:*")
    (/ . "coalton-library/classes:/")
    (> . "coalton-library/classes:>")
    (< . "coalton-library/classes:<")
    (>= . "coalton-library/classes:>=")
    (<= . "coalton-library/classes:<=")
    (== . "coalton-library/classes:==")
    (/= . "coalton-library/classes:/=")
    ;; Special forms
    (if . "coalton:if")
    ;; List operations
    (map . "coalton-library/classes:map")
    (filter . "coalton-library/list:filter")
    (fold . "coalton-library/classes:fold")
    (length . "coalton-library/list:length")
    (append . "coalton-library/list:append")
    (reverse . "coalton-library/list:reverse")
    (head . "coalton-library/list:head")
    (tail . "coalton-library/list:tail")
    ;; String operations
    (concatenate . "coalton-library/string:concat")))

(defun make-qualified-symbol (name-string)
  "Create a qualified symbol from a string like 'coalton-library/classes:+'"
  ;; Create a symbol with the full qualified name as a single string
  ;; We use vertical bars to make it a single symbol name
  (intern (format nil "|~A|" name-string) :cl-user))

(defun preprocess-coalton (code-string)
  "Transform operators to fully qualified names before Coalton sees them"
  (handler-case
      (let* ((forms (with-standard-io-syntax
                     (let ((*package* (find-package :cl-user)))
                       (read-from-string (format nil "(progn ~A)" code-string)))))
             (transformed (transform-form forms)))
        ;; Return the transformed code as a string
        (with-output-to-string (out)
          (let ((*print-case* :downcase)
                (*package* (find-package :cl-user)))
            ;; Skip the progn wrapper if it's a single form
            (if (and (eq (first transformed) 'progn)
                     (= (length (rest transformed)) 1))
                (print-form out (second transformed))
                (dolist (form (rest transformed))
                  (print-form out form)
                  (terpri out))))))
    (error (e)
      ;; If preprocessing fails, return original
      (warn "Preprocessing failed: ~A" e)
      code-string)))

(defun print-form (stream form)
  "Print a form, handling qualified symbols specially"
  (cond
    ((stringp form)
     (format stream "~S" form))
    ((and (symbolp form) 
          (char= (char (symbol-name form) 0) #\|))
     ;; This is a symbol with vertical bars like |coalton-library/classes:+|
     ;; We need to print it with bars to prevent package resolution issues
     (format stream "~A" (symbol-name form)))
    ((atom form)
     (format stream "~S" form))
    (t
     (format stream "(")
     (loop for (item . rest) on form
           do (print-form stream item)
           when rest do (format stream " "))
     (format stream ")"))))

(defun find-operator-mapping (symbol)
  "Find operator mapping by symbol name comparison"
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      (loop for (op . qualified) in *operator-mappings*
            when (string= (symbol-name op) name)
            return qualified))))

(defun transform-form (form)
  "Transform a single form, replacing operators with qualified names"
  (cond
    ;; Atom - return as is
    ((atom form) form)
    
    ;; Check if it's a list with a symbol at the head
    ((and (listp form) 
          (symbolp (first form)))
     (let ((qualified-name (find-operator-mapping (first form)))
           (first-name (symbol-name (first form))))
       (cond
         ;; Found an operator mapping
         (qualified-name
          (cons (make-qualified-symbol qualified-name)
                (mapcar #'transform-form (rest form))))
         
         ;; Special handling for define with function definition
         ((string= first-name "DEFINE")
          (if (listp (second form))
              ;; (define (name args) body) - don't transform the name/args part
              `(define ,(second form) 
                 ,@(mapcar #'transform-form (cddr form)))
              ;; (define name value)
              `(define ,(second form)
                 ,(transform-form (third form)))))
         
         ;; Special handling for let bindings
         ((string= first-name "LET")
          `(let ,(mapcar (lambda (binding)
                          (if (listp binding)
                              (list (first binding)
                                    (transform-form (second binding)))
                              binding))
                        (second form))
             ,@(mapcar #'transform-form (cddr form))))
         
         ;; Special handling for fn (lambda)
         ((string= first-name "FN")
          `(fn ,(second form)  ; Keep parameters as is
             ,@(mapcar #'transform-form (cddr form))))
         
         ;; Default: recursively transform the form
         (t (mapcar #'transform-form form)))))
    
    ;; Recursively transform other forms
    (t (mapcar #'transform-form form))))

(provide 'smelter-preprocessor)