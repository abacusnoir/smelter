(asdf:defsystem #:smelter
  :description "Industrial-strength typed scripting with Coalton"
  :author "Smelter Team"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/yourusername/smelter"
  :bug-tracker "https://github.com/yourusername/smelter/issues"
  :source-control (:git "https://github.com/yourusername/smelter.git")

  :depends-on (#:coalton
               #:uiop
               #:yason
               #:st-json
               #:drakma
               #:flexi-streams
               #:split-sequence
               #:cl-csv)

  :components ((:module "src"
                :serial t
                :components
                ((:file "coalton-translator")
                 (:file "coalton-preprocessor")

                 (:module "bridge"
                  :serial t
                  :components
                  ((:file "json")))

                 (:module "stdlib"
                  :serial t
                  :components
                  ((:file "json")))

                 (:module "adapters"
                  :serial t
                  :components
                  ((:file "fs")
                   (:file "http")
                   (:file "json-adapter")
                   (:file "process")
                   ;;(:file "cli-lib")  ; Disabled for now - needs investigation
                   ))

                 (:file "cli"))))

  :build-operation "program-op"
  :build-pathname "smt"
  :entry-point "smelter:main")
