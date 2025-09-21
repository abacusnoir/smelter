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

  ;; Helper to convert st-json objects to JSONValue
  (declare st-json-to-coalton (cl:t -> JSONValue))
  (define (st-json-to-coalton obj)
    "Convert st-json object to Coalton JSONValue"
    (lisp JSONValue (obj)
      (cl:cond
        ((cl:null obj) JSONNull)
        ((cl:eq obj cl:t) (JSONBool True))
        ((cl:eq obj cl:nil) (JSONBool False))
        ((cl:numberp obj) (JSONNumber (cl:coerce obj 'cl:double-float)))
        ((cl:stringp obj) (JSONString obj))
        ((cl:listp obj)
         ;; Check if it's an alist (object) or list (array)
         (cl:if (cl:and (cl:consp (cl:first obj))
                        (cl:stringp (cl:car (cl:first obj))))
                ;; It's an object (alist)
                (JSONObject
                 (cl:mapcar (cl:lambda (pair)
                              (Tuple (cl:car pair)
                                     (st-json-to-coalton (cl:cdr pair))))
                            obj))
                ;; It's an array (list)
                (JSONArray
                 (cl:mapcar #'st-json-to-coalton obj))))
        (cl:t (JSONString (cl:format cl:nil "~A" obj))))))

  ;; Helper to convert JSONValue to st-json objects
  (declare coalton-to-st-json (JSONValue -> cl:t))
  (define (coalton-to-st-json json-val)
    "Convert Coalton JSONValue to st-json object"
    (match json-val
      ((JSONNull) (lisp cl:t () cl:null))
      ((JSONBool True) (lisp cl:t () cl:t))
      ((JSONBool False) (lisp cl:t () cl:nil))
      ((JSONNumber n) (lisp cl:t (n) n))
      ((JSONString s) (lisp cl:t (s) s))
      ((JSONArray lst)
       (lisp cl:t (lst)
         (cl:mapcar (cl:lambda (item)
                      (coalton-to-st-json item))
                    lst)))
      ((JSONObject pairs)
       (lisp cl:t (pairs)
         (cl:mapcar (cl:lambda (pair)
                      (cl:cons (fst pair)
                               (coalton-to-st-json (snd pair))))
                    pairs)))))

  ;; Core parsing function using st-json
  (declare parse-json (String -> (Result JSONError JSONValue)))
  (define (parse-json json-str)
    "Parse a JSON string into a JSONValue using st-json"
    (lisp (Result JSONError JSONValue) (json-str)
      (cl:handler-case
          (cl:let ((parsed (st-json:read-json json-str)))
            (Ok (st-json-to-coalton parsed)))
        (cl:error (e)
          (Err (ParseError (cl:format cl:nil "JSON parse error: ~A" e)))))))

  ;; Stringify function using st-json
  (declare stringify-json (JSONValue -> String))
  (define (stringify-json json-val)
    "Convert JSONValue to JSON string using st-json"
    (lisp String (json-val)
      (cl:handler-case
          (st-json:write-json-to-string (coalton-to-st-json json-val))
        (cl:error (e)
          (cl:format cl:nil "JSON stringify error: ~A" e)))))

  ;; Path-based access with dot notation
  (declare json-get (String -> JSONValue -> (Result JSONError JSONValue)))
  (define (json-get path json-val)
    "Get value from JSON using dot-notation path (e.g., 'user.name')"
    (json-get-path (split-path path) json-val))

  ;; Helper to split path by dots
  (declare split-path (String -> (List String)))
  (define (split-path path)
    "Split 'user.name' into ['user', 'name']"
    (lisp (List String) (path)
      (cl:if (cl:string= path "")
             Nil
             (split-sequence:split-sequence #\. path :remove-empty-subseqs cl:t))))

  ;; Helper to traverse JSON path
  (declare json-get-path ((List String) -> JSONValue -> (Result JSONError JSONValue)))
  (define (json-get-path path json-val)
    "Get value by following path list"
    (match path
      ((Nil) (Ok json-val))
      ((Cons key rest)
       (match json-val
         ((JSONObject pairs)
          (match (find-object-value key pairs)
            ((Some value) (json-get-path rest value))
            ((None) (Err (KeyError (mconcat (make-list "Key not found: " key)))))))
         (_ (Err (TypeError "Path access" "Expected object")))))))

  ;; Helper to find value in object pairs
  (declare find-object-value (String -> (List (Tuple String JSONValue)) -> (Optional JSONValue)))
  (define (find-object-value key pairs)
    "Find value for key in object pairs"
    (match pairs
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (== k key)
           (Some v)
           (find-object-value key rest)))))

  ;; Type-safe getters
  (declare json-get-string (String -> JSONValue -> (Result JSONError String)))
  (define (json-get-string path json-val)
    "Get string value from JSON path"
    (match (json-get path json-val)
      ((Ok (JSONString s)) (Ok s))
      ((Ok _) (Err (TypeError path "Expected string")))
      ((Err e) (Err e))))

  (declare json-get-number (String -> JSONValue -> (Result JSONError Double-Float)))
  (define (json-get-number path json-val)
    "Get number value from JSON path"
    (match (json-get path json-val)
      ((Ok (JSONNumber n)) (Ok n))
      ((Ok _) (Err (TypeError path "Expected number")))
      ((Err e) (Err e))))

  (declare json-get-bool (String -> JSONValue -> (Result JSONError Boolean)))
  (define (json-get-bool path json-val)
    "Get boolean value from JSON path"
    (match (json-get path json-val)
      ((Ok (JSONBool b)) (Ok b))
      ((Ok _) (Err (TypeError path "Expected boolean")))
      ((Err e) (Err e))))

  ;; Constructor helpers
  (declare json-object ((List (Tuple String JSONValue)) -> JSONValue))
  (define (json-object pairs)
    "Create JSON object from pairs"
    (JSONObject pairs))

  (declare json-array ((List JSONValue) -> JSONValue))
  (define (json-array items)
    "Create JSON array from items"
    (JSONArray items)))