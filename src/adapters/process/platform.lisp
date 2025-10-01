(defpackage :smelter/adapters/process/platform
  (:documentation "
Process Adapter Platform Detection Module

Provides cross-platform abstractions for operating system detection
and platform-specific command selection.

Key Features:
- Operating system detection (Linux, MacOS, Windows, BSD)
- Platform-specific command mapping
- Cross-platform command execution

Usage:
  (use-package :smelter/adapters/process/platform)

  ;; Detect current OS
  (let ((os (detect-os)))
    (os-name os))  ;; => \"MacOS\"

  ;; Get platform-specific command
  (platform-command MacOS \"list\")  ;; => \"ls\"
  (platform-command Windows \"list\")  ;; => \"dir /b\"
")
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:lisp #:common-lisp))
  (:export
   #:OS
   #:Linux #:MacOS #:Windows #:BSD #:Unknown
   #:detect-os
   #:os-name
   #:platform-command))

(in-package :smelter/adapters/process/platform)

(coalton-toplevel
  ;; Operating System type
  (define-type OS
    "Operating system variants"
    Linux
    MacOS
    Windows
    BSD
    Unknown)

  ;; Convert OS to string name
  (declare os-name (OS -> String))
  (define (os-name os)
    "Convert OS type to string name"
    (lisp String (os)
      (lisp:cond
        ((lisp:eq os Linux) "Linux")
        ((lisp:eq os MacOS) "MacOS")
        ((lisp:eq os Windows) "Windows")
        ((lisp:eq os BSD) "BSD")
        ((lisp:eq os Unknown) "Unknown")
        (lisp:t "Unknown"))))

  ;; Detect current operating system
  (declare detect-os (Unit -> OS))
  (define (detect-os)
    "Detect the current operating system by running uname (Unix) or ver (Windows)"
    (lisp OS ()
      ;; Try Unix uname first
      (lisp:handler-case
        (lisp:multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program "uname -s"
                            :output :string
                            :error-output :string
                            :ignore-error-status lisp:t)
          (lisp:cond
            ((lisp:and (lisp:zerop exit-code) stdout)
             (lisp:cond
               ((lisp:and stdout (lisp:search "Linux" stdout)) Linux)
               ((lisp:and stdout (lisp:search "Darwin" stdout)) MacOS)  ;; macOS uses Darwin kernel
               ((lisp:and stdout (lisp:search "BSD" stdout)) BSD)
               (lisp:t Unknown)))
            ;; If uname failed, try Windows ver command
            (lisp:t
             (lisp:handler-case
               (lisp:multiple-value-bind (stdout2 stderr2 exit-code2)
                   (uiop:run-program "ver"
                                   :output :string
                                   :error-output :string
                                   :ignore-error-status lisp:t)
                 (lisp:if (lisp:zerop exit-code2)
                     Windows
                     Unknown))
               (lisp:error () Unknown)))))
        (lisp:error () Unknown))))

  ;; Get platform-specific command for common operations
  (declare platform-command (OS -> String -> String))
  (define (platform-command os intent)
    "Get platform-specific command for a given intent.

     Supported intents:
       - \"list\"   : List files in directory
       - \"copy\"   : Copy files
       - \"remove\" : Remove files
       - \"move\"   : Move files

     Returns the appropriate command for the given platform.
     "
    ;; Convert OS to string first, then match
    (let ((os-str (os-name os)))
      (lisp String (os-str intent)
        (lisp:if (lisp:string= os-str "Windows")
            ;; Windows commands
            (lisp:cond
              ((lisp:string= intent "list") "dir /b")
              ((lisp:string= intent "copy") "copy")
              ((lisp:string= intent "remove") "del")
              ((lisp:string= intent "move") "move")
              (lisp:t intent))
            ;; Unix-like commands (Linux, MacOS, BSD, Unknown)
            (lisp:cond
              ((lisp:string= intent "list") "ls")
              ((lisp:string= intent "copy") "cp")
              ((lisp:string= intent "remove") "rm")
              ((lisp:string= intent "move") "mv")
              (lisp:t intent)))))))

