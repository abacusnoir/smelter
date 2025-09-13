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

  (declare parse-json (String -> (Result JSONValue JSONError)))
  (define (parse-json json-str)
    "Parse a JSON string into a JSONValue"
    (lisp (Result JSONValue JSONError) (json-str)
      (handler-case
          (let ((parsed (st-json:read-json json-str)))
            (cl:values 
             (cl:make-instance 'coalton-prelude:Ok 
                               :ok (lisp-value-to-json-value parsed))
             cl:t))
        (cl:error (e)
          (cl:values
           (cl:make-instance 'coalton:Err
                             :err (cl:make-instance 'ParseError 
                                                    :string (cl:format cl:nil "~A" e)))
           cl:t)))))

  (declare lisp-value-to-json-value (cl:t -> JSONValue))
  (define (lisp-value-to-json-value val)
    "Convert Common Lisp JSON value to JSONValue"
    (lisp JSONValue (val)
      (cl:cond
        ((cl:null val) JSONNull)
        ((cl:eq val cl:t) (JSONBool True))
        ((cl:eq val 'st-json:json-false) (JSONBool False))
        ((cl:numberp val) (JSONNumber (cl:coerce val 'cl:double-float)))
        ((cl:stringp val) (JSONString val))
        ((cl:listp val)
         (cl:if (cl:every (cl:lambda (x) (cl:and (cl:consp x) (cl:stringp (cl:car x)))) val)
                (JSONObject (cl:mapcar (cl:lambda (pair)
                                         (cl:make-instance 'coalton:Tuple 
                                                           :fst (cl:car pair)
                                                           :snd (lisp-value-to-json-value (cl:cdr pair))))
                                       val))
                (JSONArray (cl:mapcar #'lisp-value-to-json-value val))))
        (cl:t (JSONString (cl:format cl:nil "~A" val))))))

  (declare json-value-to-lisp-value (JSONValue -> cl:t))
  (define (json-value-to-lisp-value val)
    "Convert JSONValue to Common Lisp value for st-json"
    (match val
      ((JSONNull) cl:nil)
      ((JSONBool True) cl:t)
      ((JSONBool False) 'st-json:json-false)
      ((JSONNumber n) n)
      ((JSONString s) s)
      ((JSONArray arr) (lisp cl:list (arr)
                         (cl:mapcar (cl:lambda (v) (json-value-to-lisp-value v)) arr)))
      ((JSONObject obj) (lisp cl:list (obj)
                          (cl:mapcar (cl:lambda (pair)
                                      (cl:cons (coalton:fst pair)
                                               (json-value-to-lisp-value (coalton:snd pair))))
                                     obj)))))

  (declare stringify-json (JSONValue -> String))
  (define (stringify-json json-val)
    "Convert JSONValue to JSON string"
    (lisp String (json-val)
      (st-json:write-json-to-string (json-value-to-lisp-value json-val))))

  (declare json-get (String -> JSONValue -> (Result JSONValue JSONError)))
  (define (json-get path json-val)
    "Get value at path (dot notation like 'user.name')"
    (let ((path-parts (lisp (List String) (path)
                        (split-sequence:split-sequence #\. path))))
      (json-get-path path-parts json-val)))

  (declare json-get-path ((List String) -> JSONValue -> (Result JSONValue JSONError)))
  (define (json-get-path path-parts json-val)
    "Helper for recursive path traversal"
    (match path-parts
      ((Nil) (Ok json-val))
      ((Cons key rest)
       (match json-val
         ((JSONObject obj)
          (match (find-object-key key obj)
            ((Some val) (json-get-path rest val))
            ((None) (Err (KeyError (mconcat (make-list "Key not found: " key)))))))
         (_ (Err (TypeError "Expected object" (show-json-type json-val))))))))

  (declare find-object-key (String -> (List (Tuple String JSONValue)) -> (Optional JSONValue)))
  (define (find-object-key key obj)
    "Find value by key in JSON object"
    (match obj
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (== key k)
           (Some v)
           (find-object-key key rest)))))

  (declare json-get-string (String -> JSONValue -> (Result String JSONError)))
  (define (json-get-string path json-val)
    "Get string value at path"
    (match (json-get path json-val)
      ((Ok (JSONString s)) (Ok s))
      ((Ok val) (Err (TypeError "Expected string" (show-json-type val))))
      ((Err e) (Err e))))

  (declare json-get-number (String -> JSONValue -> (Result Double-Float JSONError)))
  (define (json-get-number path json-val)
    "Get number value at path"
    (match (json-get path json-val)
      ((Ok (JSONNumber n)) (Ok n))
      ((Ok val) (Err (TypeError "Expected number" (show-json-type val))))
      ((Err e) (Err e))))

  (declare json-get-bool (String -> JSONValue -> (Result Boolean JSONError)))
  (define (json-get-bool path json-val)
    "Get boolean value at path"
    (match (json-get path json-val)
      ((Ok (JSONBool b)) (Ok b))
      ((Ok val) (Err (TypeError "Expected boolean" (show-json-type val))))
      ((Err e) (Err e))))

  (declare json-object ((List (Tuple String JSONValue)) -> JSONValue))
  (define (json-object pairs)
    "Create JSON object from key-value pairs"
    (JSONObject pairs))

  (declare json-array ((List JSONValue) -> JSONValue))
  (define (json-array values)
    "Create JSON array from list of values"
    (JSONArray values))

  (declare show-json-type (JSONValue -> String))
  (define (show-json-type val)
    "Get string representation of JSON value type"
    (match val
      ((JSONNull) "null")
      ((JSONBool _) "boolean")
      ((JSONNumber _) "number")  
      ((JSONString _) "string")
      ((JSONArray _) "array")
      ((JSONObject _) "object"))))