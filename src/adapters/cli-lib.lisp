(defpackage #:smelter/adapters/cli
  (:use #:coalton #:coalton-prelude)
  (:export
   #:ArgType #:StringArg #:IntegerArg #:BooleanArg #:ListArg
   #:ArgSpec
   #:Args
   #:CLIError #:MissingRequired #:InvalidType
   #:parse-args
   #:get-arg
   #:get-arg-string
   #:get-arg-integer
   #:get-arg-boolean
   #:string-arg
   #:integer-arg
   #:boolean-arg
   #:list-arg
   #:generate-help))

(in-package #:smelter/adapters/cli)

(coalton-toplevel
  (define-type ArgType
    StringArg
    IntegerArg
    BooleanArg
    (ListArg ArgType))

  (define-type ArgSpec
    (ArgSpec String String ArgType Boolean (Optional String)))  ; name, description, type, required, default

  (define-type Args
    (Args (List (Tuple String String))))  ; Parsed argument key-value pairs

  (define-type CLIError
    (MissingRequired String)
    (InvalidType String String String))  ; arg-name, expected-type, actual-value

  ;; Constructor helpers
  (declare string-arg (String -> String -> Boolean -> ArgSpec))
  (define (string-arg name description required)
    "Create string argument specification"
    (ArgSpec name description StringArg required None))

  (declare integer-arg (String -> String -> Boolean -> ArgSpec))
  (define (integer-arg name description required)
    "Create integer argument specification"
    (ArgSpec name description IntegerArg required None))

  (declare boolean-arg (String -> String -> Boolean -> ArgSpec))
  (define (boolean-arg name description required)
    "Create boolean argument specification"
    (ArgSpec name description BooleanArg required None))

  (declare list-arg (ArgType -> String -> String -> Boolean -> ArgSpec))
  (define (list-arg element-type name description required)
    "Create list argument specification"
    (ArgSpec name description (ListArg element-type) required None))

  ;; Argument parsing
  (declare parse-args ((List ArgSpec) -> (List String) -> (Result CLIError Args)))
  (define (parse-args specs raw-args)
    "Parse command line arguments according to specifications"
    (match (parse-raw-args raw-args)
      ((Ok parsed-pairs)
       (match (validate-args specs parsed-pairs)
         ((Ok validated) (Ok (Args validated)))
         ((Err e) (Err e))))
      ((Err e) (Err e))))

  ;; Helper to parse raw arguments into key-value pairs
  (declare parse-raw-args ((List String) -> (Result CLIError (List (Tuple String String)))))
  (define (parse-raw-args raw-args)
    "Parse raw argument list into key-value pairs"
    (parse-args-loop raw-args Nil))

  (declare parse-args-loop ((List String) -> (List (Tuple String String)) -> (Result CLIError (List (Tuple String String)))))
  (define (parse-args-loop remaining acc)
    "Loop through arguments building key-value pairs"
    (match remaining
      ((Nil) (Ok (reverse acc)))
      ((Cons arg rest)
       (if (starts-with-dash arg)
           (let ((key (remove-dashes arg)))
             (match rest
               ((Nil) 
                ;; Boolean flag without value
                (parse-args-loop Nil (Cons (Tuple key "true") acc)))
               ((Cons value rest2)
                (if (starts-with-dash value)
                    ;; Boolean flag, next arg is another flag
                    (parse-args-loop rest (Cons (Tuple key "true") acc))
                    ;; Key-value pair
                    (parse-args-loop rest2 (Cons (Tuple key value) acc))))))
           ;; Positional argument - skip for now
           (parse-args-loop rest acc)))))

  ;; Helper to check if string starts with dash
  (declare starts-with-dash (String -> Boolean))
  (define (starts-with-dash s)
    "Check if string starts with - or --"
    (lisp Boolean (s)
      (cl:and (cl:> (cl:length s) 0)
              (cl:char= (cl:char s 0) #\-))))

  ;; Helper to remove leading dashes
  (declare remove-dashes (String -> String))
  (define (remove-dashes s)
    "Remove leading dashes from argument"
    (lisp String (s)
      (cl:string-left-trim "-" s)))

  ;; Validate parsed arguments against specifications
  (declare validate-args ((List ArgSpec) -> (List (Tuple String String)) -> (Result CLIError (List (Tuple String String)))))
  (define (validate-args specs parsed)
    "Validate parsed arguments against specifications"
    (validate-loop specs parsed Nil))

  (declare validate-loop ((List ArgSpec) -> (List (Tuple String String)) -> (List (Tuple String String)) -> (Result CLIError (List (Tuple String String)))))
  (define (validate-loop specs parsed acc)
    "Validation loop"
    (match specs
      ((Nil) (Ok acc))
      ((Cons spec rest-specs)
       (match spec
         ((ArgSpec name _ arg-type required default)
          (match (find-arg name parsed)
            ((Some value)
             ;; Found the argument, validate type
             (match (validate-type arg-type value)
               ((Ok validated-value)
                (validate-loop rest-specs parsed (Cons (Tuple name validated-value) acc)))
               ((Err e) (Err e))))
            ((None)
             ;; Argument not found
             (if required
                 (Err (MissingRequired name))
                 (match default
                   ((Some def-val)
                    (validate-loop rest-specs parsed (Cons (Tuple name def-val) acc)))
                   ((None)
                    (validate-loop rest-specs parsed acc)))))))))))

  ;; Find argument value by name
  (declare find-arg (String -> (List (Tuple String String)) -> (Optional String)))
  (define (find-arg name pairs)
    "Find argument value by name"
    (match pairs
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (== k name)
           (Some v)
           (find-arg name rest)))))

  ;; Type validation
  (declare validate-type (ArgType -> String -> (Result CLIError String)))
  (define (validate-type arg-type value)
    "Validate value against argument type"
    (match arg-type
      ((StringArg) (Ok value))
      ((IntegerArg)
       (lisp (Result CLIError String) (value)
         (cl:handler-case
             (cl:progn
               (cl:parse-integer value)
               (Ok value))
           (cl:error () (Err (InvalidType "integer" value "not a valid integer"))))))
      ((BooleanArg)
       (if (or (== value "true") (== value "false") (== value "1") (== value "0"))
           (Ok value)
           (Err (InvalidType "boolean" value "not a valid boolean"))))
      ((ListArg _)
       ;; For now, treat as string - could be enhanced to parse lists
       (Ok value))))

  ;; Argument accessors
  (declare get-arg (Args -> String -> (Optional String)))
  (define (get-arg args name)
    "Get argument value by name"
    (match args
      ((Args pairs) (find-arg name pairs))))

  (declare get-arg-string (Args -> String -> (Result CLIError String)))
  (define (get-arg-string args name)
    "Get string argument value"
    (match (get-arg args name)
      ((Some value) (Ok value))
      ((None) (Err (MissingRequired name)))))

  (declare get-arg-integer (Args -> String -> (Result CLIError Integer)))
  (define (get-arg-integer args name)
    "Get integer argument value"
    (match (get-arg args name)
      ((Some value)
       (lisp (Result CLIError Integer) (value)
         (cl:handler-case
             (Ok (cl:parse-integer value))
           (cl:error () (Err (InvalidType name "integer" value))))))
      ((None) (Err (MissingRequired name)))))

  (declare get-arg-boolean (Args -> String -> (Result CLIError Boolean)))
  (define (get-arg-boolean args name)
    "Get boolean argument value"
    (match (get-arg args name)
      ((Some value)
       (cond
         ((or (== value "true") (== value "1")) (Ok True))
         ((or (== value "false") (== value "0")) (Ok False))
         (True (Err (InvalidType name "boolean" value)))))
      ((None) (Err (MissingRequired name)))))

  ;; Help generation
  (declare generate-help ((List ArgSpec) -> String))
  (define (generate-help specs)
    "Generate help text from argument specifications"
    (lisp String (specs)
      (cl:with-output-to-string (s)
        (cl:format s "Usage:~%")
        (cl:dolist (spec specs)
          (cl:destructuring-bind (name description arg-type required default) spec
            (cl:format s "  --~A: ~A (~A)~A~%"
                       name
                       description
                       (cl:case arg-type
                         (StringArg "string")
                         (IntegerArg "integer")
                         (BooleanArg "boolean")
                         (cl:t "unknown"))
                       (cl:if required " [required]" ""))))))))