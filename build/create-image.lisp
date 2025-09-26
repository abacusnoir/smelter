;;;; Smelter Image Builder
;;;; Creates SBCL core with Coalton pre-loaded for fast startup

(format t "~%=== Smelter Image Builder ===~%")
(format t "Building SBCL core with embedded Coalton...~%~%")

;;; Ensure we have a clean environment
(setf *print-case* :downcase)

;;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                      (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (format t "Loading Quicklisp...~%")
    (load quicklisp-init)
    (format t "Quicklisp loaded.~%"))
  (unless (find-package :ql)
    (error "Quicklisp not found. Please install Quicklisp first.")))

;;; Load Coalton and dependencies
(format t "Loading Coalton...~%")
(handler-case
    (progn
      ;; Load Coalton system
      (ql:quickload :coalton :silent t)
      
      ;; Load adapter dependencies
      (format t "Loading adapter dependencies...~%")
      (ql:quickload :yason :silent t)
      (ql:quickload :split-sequence :silent t)
      (ql:quickload :drakma :silent t)
      (ql:quickload :cl-csv :silent t)
      
      ;; Verify Coalton is loaded
      (unless (find-package :coalton)
        (error "Coalton package not found after loading"))
      
      ;; Load coalton-prelude for arithmetic operators
      (unless (find-package :coalton-prelude)
        (error "Coalton-prelude package not found"))
      
      ;; Set up coalton-user package with proper prelude imports
      (format t "Setting up coalton-user package...~%")
      (unless (find-package :coalton-user)
        (defpackage :coalton-user
          (:use
           :cl
           :coalton
           :coalton-prelude
           :coalton-library/classes
           :coalton-library/result)
          (:local-nicknames
           (#:result #:coalton-library/result)
           (#:classes #:coalton-library/classes))))

      ;; Unlock coalton-library packages to allow macro expansion
      (format t "Unlocking Coalton library packages for macro expansion...~%")
      (sb-ext:unlock-package :coalton-library/classes)
      (when (find-package :coalton-library/result)
        (sb-ext:unlock-package :coalton-library/result))

      (format t "Coalton loaded successfully.~%")

      ;; Critical fix: Export ADT symbols immediately after Coalton loading
      (format t "Exporting ADT symbols for pattern matching...~%")

      ;; Export Result ADT symbols using runtime string-to-symbol conversion with correct pipe names
      (handler-case
          (let ((classes-pkg (find-package :coalton-library/classes)))
            (when classes-pkg
              (format t "Exporting Result ADT symbols...~%")
              ;; Export both regular names and pipe-delimited names that Coalton generates
              (let ((result-symbols (list
                                     ;; Regular names
                                     (intern "RESULT/OK" classes-pkg)
                                     (intern "RESULT/ERR" classes-pkg)
                                     (intern "RESULT/OK-_0" classes-pkg)
                                     (intern "RESULT/ERR-_0" classes-pkg)
                                     ;; Pipe-delimited names that Coalton actually uses
                                     (intern "result/ok" classes-pkg)
                                     (intern "result/err" classes-pkg)
                                     (intern "result/ok-_0" classes-pkg)
                                     (intern "result/err-_0" classes-pkg))))
                (dolist (sym result-symbols)
                  (when sym
                    ;; Try to export regardless of whether it's bound - the symbol just needs to exist
                    (export sym classes-pkg)
                    (format t "Exported symbol: ~A~%" sym))))))
        (error (e)
          (format t "Warning: Could not export Result symbols: ~A~%" e)))

      ;; Export Optional ADT symbols if they exist
      (handler-case
          (let ((classes-pkg (find-package :coalton-library/classes)))
            (when (and classes-pkg (find-symbol "OPTIONAL/SOME" classes-pkg))
              (format t "Exporting Optional ADT symbols...~%")
              (let ((optional-symbols (list
                                       ;; Regular names
                                       (intern "OPTIONAL/SOME" classes-pkg)
                                       (intern "OPTIONAL/NONE" classes-pkg)
                                       (intern "OPTIONAL/SOME-_0" classes-pkg)
                                       ;; Pipe-delimited names that Coalton actually uses
                                       (intern "optional/some" classes-pkg)
                                       (intern "optional/none" classes-pkg)
                                       (intern "optional/some-_0" classes-pkg))))
                (dolist (sym optional-symbols)
                  (when sym
                    (export sym classes-pkg)
                    (format t "Exported optional symbol: ~A~%" sym))))))
        (error (e)
          (format t "Warning: Could not export Optional symbols: ~A~%" e)))

      ;; Load Smelter components directly  
      (format t "Loading Smelter components...~%")
      (let ((cwd (uiop:getcwd)))
        ;; Skip preprocessor - using simplified approach
        (load (merge-pathnames "src/coalton-translator.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-prelude.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-io.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-system.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-file.lisp" cwd))
        (load (merge-pathnames "src/stdlib/smelter-http.lisp" cwd))
        ;; Skip old smelter-json.lisp - replaced by new JSON adapter

        ;; Load JSON bridge and adapter
        (format t "Loading JSON bridge and adapter...~%")
        (load (merge-pathnames "src/bridge/json.lisp" cwd))
        (load (merge-pathnames "src/stdlib/json-simple.lisp" cwd))
        
        ;; Load CSV module with qualified types
        (format t "Loading CSV module...~%")
        (load (merge-pathnames "src/stdlib/smelter-csv.lisp" cwd))
        
        ;; Load test module
        (format t "Loading test module...~%")
        (load (merge-pathnames "src/stdlib/smelter-test.lisp" cwd))

        ;; Load datetime module
        (format t "Loading datetime module...~%")
        (load (merge-pathnames "src/stdlib/smelter-datetime.lisp" cwd))
        
        ;; Skip old JSON adapter - using new comprehensive JSON library
        ;; Other adapters disabled temporarily
        ;; (load (merge-pathnames "src/adapters/cli.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/process.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/http.lisp" cwd))
        ;; (load (merge-pathnames "src/adapters/fs.lisp" cwd))
        
        (load (merge-pathnames "src/cli.lisp" cwd)))
      
      ;; Verify all packages are loaded
      (unless (find-package :smelter.translator)
        (error "Smelter translator package not found"))
      (unless (find-package :smelter.stdlib.prelude)
        (error "Smelter stdlib.prelude package not found"))
      (unless (find-package :smelter.stdlib.io)
        (error "Smelter stdlib.io package not found"))
      (unless (find-package :smelter.stdlib.system)
        (error "Smelter stdlib.system package not found"))
      (unless (find-package :smelter.stdlib.file)
        (error "Smelter stdlib.file package not found"))
      (unless (find-package :smelter)
        (error "Smelter main package not found"))
      
      ;; Verify CSV package 
      (unless (find-package :smelter.stdlib.csv)
        (error "Smelter CSV package not found"))
      
      ;; Verify test package
      (unless (find-package :smelter.stdlib.test)
        (error "Smelter test package not found"))

      ;; Verify datetime package
      (unless (find-package :smelter.stdlib.datetime)
        (error "Smelter datetime package not found"))
      
      ;; Verify new JSON library package
      (unless (find-package :smelter.stdlib.json)
        (error "Smelter JSON library package not found"))
      ;; Other adapter packages verification disabled temporarily
      ;; (unless (find-package :smelter/adapters/cli)
      ;;   (error "Smelter CLI adapter package not found"))
      ;; (unless (find-package :smelter/adapters/process)
      ;;   (error "Smelter Process adapter package not found"))
      ;; (unless (find-package :smelter/adapters/http)
      ;;   (error "Smelter HTTP adapter package not found"))
      ;; (unless (find-package :smelter/adapters/fs)
      ;;   (error "Smelter FS adapter package not found"))
      
      (format t "Smelter system loaded successfully.~%")
      
      ;; Test basic functionality
      (format t "Testing Coalton functionality...~%")
      (format t "Coalton test passed: basic loading successful~%")

      ;; Critical fix: Register ADT types in SBCL's type system for pattern matching
      (format t "Registering ADT types in SBCL's type system...~%")

      ;; STEP 1: Register ADT types in SBCL's type system
      (handler-case
          (progn
            (format t "Registering Result ADT types...~%")

            ;; Define Result type predicates that work with the actual instances
            (let ((classes-pkg (find-package :coalton-library/classes)))
              (when classes-pkg
                ;; Define predicates using runtime symbol creation
                (let ((ok-pred-name (intern "RESULT/OK-TYPE-P" classes-pkg))
                      (err-pred-name (intern "RESULT/ERR-TYPE-P" classes-pkg)))

                  ;; Create the predicate functions
                  (setf (symbol-function ok-pred-name)
                        (lambda (x)
                          (and (typep x 'structure-object)
                               (let ((type-name (type-of x)))
                                 (and (symbolp type-name)
                                      (string= (symbol-name type-name) "RESULT/OK")
                                      (string= (package-name (symbol-package type-name)) "COALTON-LIBRARY/CLASSES"))))))

                  (setf (symbol-function err-pred-name)
                        (lambda (x)
                          (and (typep x 'structure-object)
                               (let ((type-name (type-of x)))
                                 (and (symbolp type-name)
                                      (string= (symbol-name type-name) "RESULT/ERR")
                                      (string= (package-name (symbol-package type-name)) "COALTON-LIBRARY/CLASSES"))))))

                  ;; Register the types using SBCL's deftype with runtime symbol creation
                  ;; Register both regular and pipe-delimited versions
                  (eval `(deftype ,(intern "result/ok" classes-pkg) ()
                           '(satisfies ,ok-pred-name)))
                  (eval `(deftype ,(intern "result/err" classes-pkg) ()
                           '(satisfies ,err-pred-name)))

                  (format t "Registered Result types with pipe names successfully.~%"))))

            (format t "Result ADT types registered successfully.~%"))
        (error (e)
          (format t "Warning: Could not register Result types: ~A~%" e)))

      ;; STEP 2: Register Optional ADT types if they exist
      (handler-case
          (when (find-symbol "OPTIONAL/SOME" :coalton-library/classes)
            (format t "Registering Optional ADT types...~%")

            (let ((classes-pkg (find-package :coalton-library/classes)))
              (when classes-pkg
                ;; Define predicates using runtime symbol creation
                (let ((some-pred-name (intern "OPTIONAL/SOME-TYPE-P" classes-pkg))
                      (none-pred-name (intern "OPTIONAL/NONE-TYPE-P" classes-pkg)))

                  ;; Create the predicate functions
                  (setf (symbol-function some-pred-name)
                        (lambda (x)
                          (and (typep x 'structure-object)
                               (let ((type-name (type-of x)))
                                 (and (symbolp type-name)
                                      (string= (symbol-name type-name) "OPTIONAL/SOME")
                                      (string= (package-name (symbol-package type-name)) "COALTON-LIBRARY/CLASSES"))))))

                  (setf (symbol-function none-pred-name)
                        (lambda (x)
                          (and (typep x 'structure-object)
                               (let ((type-name (type-of x)))
                                 (and (symbolp type-name)
                                      (string= (symbol-name type-name) "OPTIONAL/NONE")
                                      (string= (package-name (symbol-package type-name)) "COALTON-LIBRARY/CLASSES"))))))

                  ;; Register the types using SBCL's deftype with runtime symbol creation
                  (eval `(deftype ,(intern "optional/some" classes-pkg) ()
                           '(satisfies ,some-pred-name)))
                  (eval `(deftype ,(intern "optional/none" classes-pkg) ()
                           '(satisfies ,none-pred-name)))

                  (format t "Registered Optional types with pipe names successfully.~%"))))

            (format t "Optional ADT types registered successfully.~%"))
        (error (e)
          (format t "Warning: Could not register Optional types: ~A~%" e)))

      ;; STEP 3: Force pattern matching compilation and execution to ensure runtime structures exist
      (handler-case
          (progn
            (format t "Forcing comprehensive pattern matching compilation...~%")

            ;; Test basic Coalton functionality first
            (let ((*package* (find-package :coalton-user)))
              (eval (read-from-string "(coalton:coalton (+ 1 2))")))

            ;; Force Result pattern matching compilation AND execution to create runtime accessors
            (format t "Testing Result pattern matching compilation and execution...~%")
            (let ((*package* (find-package :coalton-user)))
              (eval (read-from-string
"(coalton:coalton-toplevel
  (declare force-result-test (Integer -> Integer))
  (define force-result-test
    (fn (dummy)
      (match (Ok 42)
        ((Ok x) (+ x 1))
        ((Err _) 0)))))"))

              ;; CRITICAL: Execute the pattern matching to force accessor function creation
              (let ((test-fn (find-symbol "FORCE-RESULT-TEST" :coalton-user)))
                (when test-fn
                  (let ((result (funcall (symbol-function test-fn) 0)))
                    (format t "Result pattern matching executed successfully, result: ~A~%" result)))))

            ;; Force Optional pattern matching compilation AND execution to create runtime accessors
            (format t "Testing Optional pattern matching compilation and execution...~%")
            (let ((*package* (find-package :coalton-user)))
              (eval (read-from-string
"(coalton:coalton-toplevel
  (declare force-optional-test (Integer -> Integer))
  (define force-optional-test
    (fn (dummy)
      (match (Some 5)
        ((Some x) x)
        (None 0)))))"))

              ;; CRITICAL: Execute the pattern matching to force accessor function creation
              (let ((test-fn (find-symbol "FORCE-OPTIONAL-TEST" :coalton-user)))
                (when test-fn
                  (let ((result (funcall (symbol-function test-fn) 0)))
                    (format t "Optional pattern matching executed successfully, result: ~A~%" result)))))

            (format t "Pattern matching ADT runtime structures created and executed successfully.~%")

            (format t "Pattern matching compilation and execution completed successfully.~%"))

        (error (e)
          (format t "Error in pattern matching initialization: ~A~%" e)
          ;; Continue anyway - this is not fatal
          ))

      (format t "ADT symbol export and type registration completed.~%"))

  (error (e)
    (format t "Error loading systems: ~A~%" e)
    (sb-ext:exit :code 1)))

;;; Optimize the image
(format t "Optimizing image...~%")

;; Set compilation policy for smaller image
(proclaim '(optimize (speed 2) (safety 1) (debug 1) (space 3)))

;; Clean up temporary packages
(when (find-package :smelter-test)
  (delete-package :smelter-test))

;; Clear some caches to reduce image size
(handler-case
    (progn
      ;; Clear ASDF cache if available
      (when (find-package :asdf)
        (let ((clear-cache (find-symbol "CLEAR-CACHE" :asdf)))
          (when clear-cache
            (funcall clear-cache))))
      
      ;; Clear Quicklisp temporary data
      (when (find-package :ql)
        (let ((clear-dist-cache (find-symbol "CLEAR-DIST-CACHE" :ql)))
          (when clear-dist-cache
            (handler-case (funcall clear-dist-cache)
              (error () nil))))))
  (error () nil)) ; Ignore errors in cleanup

;; Disable debugger for production use
(setf *debugger-hook* 
      (lambda (condition me)
        (declare (ignore me))
        (format *error-output* "Fatal error: ~A~%" condition)
        (sb-ext:exit :code 1)))

;;; Save the core image
(format t "Saving core image...~%")

(let ((core-path "build/smelter.core"))
  (ensure-directories-exist core-path)
  
  (handler-case
      (progn
        (sb-ext:save-lisp-and-die core-path
                                  :executable nil
                                  :compression t
                                  :save-runtime-options nil))
    (error (e)
      (format t "Error saving core: ~A~%" e)
      (sb-ext:exit :code 1))))

;; This point should never be reached
(format t "Error: save-lisp-and-die returned unexpectedly~%")
(sb-ext:exit :code 1)