(require 'asdf)

(push #P"/Users/agam/Projects/smelter/" asdf:*central-registry*)

;; Load the smelter system to get all the dependencies loaded
(asdf:load-system :smelter)

(defun collect-system-files (system)
  (let ((files '()))
    (labels ((collect (component)
               (typecase component
                 (asdf:c-source-file (push (asdf:component-pathname component) files))
                 (asdf:cl-source-file (push (asdf:component-pathname component) files))
                 (asdf:static-file (push (asdf:component-pathname component) files))
                 (asdf:module (dolist (c (asdf:component-children component))
                                (collect c))))))
      (collect (asdf:find-system system)))
    (reverse files)))

;; Now, manually process and eval all coalton code to ensure
;; ADT accessors are available at runtime.
(let ((files (collect-system-files :smelter)))
  (dolist (file files)
    (let* ((content (uiop:read-file-string file))
           (preprocessed (smelter.preprocessor:preprocess-coalton content)))
      (eval (read-from-string preprocessed)))))

;; Dump the executable
(sb-ext:save-lisp-and-die "/Users/agam/Projects/smelter/smelter-core"
                          :toplevel 'smelter:main
                          :executable t)

(uiop:quit)
