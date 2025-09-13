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
    (ArgSpec String String ArgType Boolean (Optional String)))

  (define-type Args
    (Args (List (Tuple String String))))

  (define-type CLIError
    (MissingRequired String)
    (InvalidType String String String))

  (declare parse-args ((List ArgSpec) -> (List String) -> (Result Args CLIError)))
  (define (parse-args specs arg-list)
    "Parse command line arguments according to specifications"
    (parse-args-helper specs arg-list (make-list)))

  (declare parse-args-helper ((List ArgSpec) -> (List String) -> (List (Tuple String String)) -> (Result Args CLIError)))
  (define (parse-args-helper specs remaining-args parsed-args)
    "Helper function for recursive argument parsing"
    (match remaining-args
      ((Nil) 
       (match (check-required-args specs parsed-args)
         ((Ok _) (Ok (Args parsed-args)))
         ((Err e) (Err e))))
      ((Cons arg rest)
       (if (string-starts-with? "--" arg)
           (match (parse-long-option arg rest specs)
             ((Ok (Tuple key value new-rest))
              (parse-args-helper specs new-rest (cons (Tuple key value) parsed-args)))
             ((Err e) (Err e)))
           (if (string-starts-with? "-" arg)
               (match (parse-short-option arg rest specs)
                 ((Ok (Tuple key value new-rest))
                  (parse-args-helper specs new-rest (cons (Tuple key value) parsed-args)))
                 ((Err e) (Err e)))
               (parse-args-helper specs rest parsed-args))))))

  (declare parse-long-option (String -> (List String) -> (List ArgSpec) -> (Result (Tuple String String (List String)) CLIError)))
  (define (parse-long-option arg rest specs)
    "Parse --long-name style option"
    (let ((option-name (string-drop 2 arg)))
      (match (find-arg-spec option-name specs)
        ((Some spec)
         (match spec
           ((ArgSpec name _ arg-type _ _)
            (match arg-type
              ((BooleanArg)
               (Ok (Tuple name "true" rest)))
              (_
               (match rest
                 ((Nil) (Err (InvalidType name "Missing value" "")))
                 ((Cons value new-rest)
                  (match (validate-arg-type value arg-type)
                    ((Ok validated-value) (Ok (Tuple name validated-value new-rest)))
                    ((Err e) (Err e))))))))))
        ((None) (Err (InvalidType option-name "Unknown option" ""))))))

  (declare parse-short-option (String -> (List String) -> (List ArgSpec) -> (Result (Tuple String String (List String)) CLIError)))
  (define (parse-short-option arg rest specs)
    "Parse -s style option (simplified - maps to long name)"
    (let ((short-name (string-drop 1 arg)))
      (parse-long-option (mconcat (make-list "--" short-name)) rest specs)))

  (declare find-arg-spec (String -> (List ArgSpec) -> (Optional ArgSpec)))
  (define (find-arg-spec name specs)
    "Find argument specification by name"
    (match specs
      ((Nil) None)
      ((Cons spec rest)
       (match spec
         ((ArgSpec spec-name _ _ _ _)
          (if (== name spec-name)
              (Some spec)
              (find-arg-spec name rest)))))))

  (declare validate-arg-type (String -> ArgType -> (Result String CLIError)))
  (define (validate-arg-type value arg-type)
    "Validate and convert argument value according to type"
    (match arg-type
      ((StringArg) (Ok value))
      ((IntegerArg)
       (match (string-to-integer value)
         ((Some _) (Ok value))
         ((None) (Err (InvalidType value "Expected integer" "")))))
      ((BooleanArg)
       (if (or (== value "true") (== value "false") (== value "yes") (== value "no"))
           (Ok (if (or (== value "true") (== value "yes")) "true" "false"))
           (Err (InvalidType value "Expected boolean (true/false/yes/no)" ""))))
      ((ListArg _) (Ok value))))

  (declare check-required-args ((List ArgSpec) -> (List (Tuple String String)) -> (Result Unit CLIError)))
  (define (check-required-args specs parsed-args)
    "Check that all required arguments are present"
    (match specs
      ((Nil) (Ok Unit))
      ((Cons spec rest)
       (match spec
         ((ArgSpec name _ _ required _)
          (if (and required (not (arg-present? name parsed-args)))
              (Err (MissingRequired name))
              (check-required-args rest parsed-args)))))))

  (declare arg-present? (String -> (List (Tuple String String)) -> Boolean))
  (define (arg-present? name parsed-args)
    "Check if argument is present in parsed arguments"
    (match parsed-args
      ((Nil) False)
      ((Cons (Tuple arg-name _) rest)
       (if (== name arg-name)
           True
           (arg-present? name rest)))))

  (declare get-arg (Args -> String -> (Optional String)))
  (define (get-arg args name)
    "Get argument value by name"
    (match args
      ((Args arg-list) (get-arg-from-list name arg-list))))

  (declare get-arg-from-list (String -> (List (Tuple String String)) -> (Optional String)))
  (define (get-arg-from-list name arg-list)
    "Helper to get argument from list"
    (match arg-list
      ((Nil) None)
      ((Cons (Tuple arg-name value) rest)
       (if (== name arg-name)
           (Some value)
           (get-arg-from-list name rest)))))

  (declare get-arg-string (Args -> String -> (Result String CLIError)))
  (define (get-arg-string args name)
    "Get string argument value"
    (match (get-arg args name)
      ((Some value) (Ok value))
      ((None) (Err (MissingRequired name)))))

  (declare get-arg-integer (Args -> String -> (Result Integer CLIError)))
  (define (get-arg-integer args name)
    "Get integer argument value"
    (match (get-arg args name)
      ((Some value)
       (match (string-to-integer value)
         ((Some i) (Ok i))
         ((None) (Err (InvalidType name value "Expected integer")))))
      ((None) (Err (MissingRequired name)))))

  (declare get-arg-boolean (Args -> String -> (Result Boolean CLIError)))
  (define (get-arg-boolean args name)
    "Get boolean argument value"
    (match (get-arg args name)
      ((Some value)
       (if (== value "true")
           (Ok True)
           (Ok False)))
      ((None) (Err (MissingRequired name)))))

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
  (define (list-arg inner-type name description required)
    "Create list argument specification"
    (ArgSpec name description (ListArg inner-type) required None))

  (declare generate-help ((List ArgSpec) -> String))
  (define (generate-help specs)
    "Generate help text from argument specifications"
    (mconcat (cons "Usage: \n" (map format-arg-spec specs))))

  (declare format-arg-spec (ArgSpec -> String))
  (define (format-arg-spec spec)
    "Format single argument specification for help text"
    (match spec
      ((ArgSpec name description arg-type required _)
       (let ((type-str (format-arg-type arg-type))
             (req-str (if required " (required)" " (optional)")))
         (mconcat (make-list "  --" name " " type-str "  " description req-str "\n"))))))

  (declare format-arg-type (ArgType -> String))
  (define (format-arg-type arg-type)
    "Format argument type for help text"
    (match arg-type
      ((StringArg) "<string>")
      ((IntegerArg) "<integer>")
      ((BooleanArg) "")
      ((ListArg inner-type) (mconcat (make-list "<list of " (format-arg-type inner-type) ">")))))

  (declare string-starts-with? (String -> String -> Boolean))
  (define (string-starts-with? prefix str)
    "Check if string starts with prefix"
    (lisp Boolean (prefix str)
      (cl:and (cl:>= (cl:length str) (cl:length prefix))
              (cl:string= prefix str :end2 (cl:length prefix)))))

  (declare string-drop (Integer -> String -> String))
  (define (string-drop n str)
    "Drop first n characters from string"
    (lisp String (n str)
      (cl:subseq str n)))

  (declare string-to-integer (String -> (Optional Integer)))
  (define (string-to-integer str)
    "Parse string as integer"
    (lisp (Optional Integer) (str)
      (cl:handler-case
          (cl:let ((parsed (cl:parse-integer str)))
            (cl:make-instance 'coalton:Some :some parsed))
        (cl:error ()
          (cl:make-instance 'coalton:None))))))