(defpackage #:smelter/adapters/json
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:cp #:coalton-prelude))
  (:export
   #:JSONValue
   #:JSONNull #:JSONBool #:JSONNumber #:JSONString #:JSONArray #:JSONObject
   #:JSONError
   #:ParseError #:KeyError #:TypeError
   #:parse-json
   #:stringify-json
   #:json-get
   #:json-get-string
   #:json-get-number  
   #:json-get-bool
   #:json-object
   #:json-array))

(in-package #:smelter/adapters/json)

(coalton-toplevel
  ;; Type definitions
  (define-type JSONValue
    JSONNull
    (JSONBool Boolean)
    (JSONNumber Double-Float)
    (JSONString String)
    (JSONArray (List JSONValue))
    (JSONObject (List (Tuple String JSONValue))))

  (define-type JSONError
    (ParseError String)
    (KeyError String) 
    (TypeError String String))

  ;; Core parsing function
  (declare parse-json (String -> (Result JSONError JSONValue)))
  (define (parse-json json-str)
    "Parse a JSON string into a JSONValue - basic implementation"
    (cond
      ((== json-str "null") (Ok JSONNull))
      ((== json-str "true") (Ok (JSONBool True)))
      ((== json-str "false") (Ok (JSONBool False)))
      ((== json-str "\"\"") (Ok (JSONString "")))
      (True (Err (ParseError "Complex JSON not yet supported")))))

  ;; Stringify function
  (declare stringify-json (JSONValue -> String))
  (define (stringify-json json-val)
    "Convert JSONValue to JSON string - simplified version"
    (match json-val
      ((JSONNull) "null")
      ((JSONBool b) (if b "true" "false"))
      ((JSONNumber n) (lisp String (n) (cl:format cl:nil "~A" n)))
      ((JSONString s) (lisp String (s) (cl:format cl:nil "\"~A\"" s)))
      ((JSONArray _) "[]")
      ((JSONObject _) "{}")))

  ;; Path access functions
  (declare json-get (String -> JSONValue -> (Result JSONError JSONValue)))
  (define (json-get path json-val)
    "Get value at path - basic implementation"
    (if (== path "")
        (Ok json-val)
        (Err (KeyError "Path access not yet supported"))))

  (declare json-get-string (String -> JSONValue -> (Result JSONError String)))
  (define (json-get-string path json-val)
    "Get string value at path - basic implementation"
    (match json-val
      ((JSONString s) (Ok s))
      (_ (Err (TypeError "Expected string" "got non-string")))))

  (declare json-get-number (String -> JSONValue -> (Result JSONError Double-Float)))
  (define (json-get-number path json-val)
    "Get number value at path - basic implementation"
    (match json-val
      ((JSONNumber n) (Ok n))
      (_ (Err (TypeError "Expected number" "got non-number")))))

  (declare json-get-bool (String -> JSONValue -> (Result JSONError Boolean)))
  (define (json-get-bool path json-val)
    "Get boolean value at path - basic implementation"
    (match json-val
      ((JSONBool b) (Ok b))
      (_ (Err (TypeError "Expected boolean" "got non-boolean")))))

  ;; Constructor functions
  (declare json-object ((List (Tuple String JSONValue)) -> JSONValue))
  (define (json-object pairs)
    "Create JSON object from key-value pairs"
    (JSONObject pairs))

  (declare json-array ((List JSONValue) -> JSONValue))
  (define (json-array values)
    "Create JSON array from list of values"
    (JSONArray values)))