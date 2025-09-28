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


  ;; Helper function to convert st-json objects to JSONValue
  (declare st-json-to-coalton-value (String -> (Result JSONError JSONValue)))
  (define (st-json-to-coalton-value obj-str)
    "Convert a JSON string to JSONValue by parsing it first"
    (lisp (Result JSONError JSONValue) (obj-str)
      (cl:handler-case
          (cl:let ((obj (st-json:read-json obj-str)))
            (cl:labels ((convert-obj (x)
                          (cl:cond
                            ((cl:null x) JSONNull)
                            ((cl:eq x cl:t) (JSONBool True))
                            ((cl:eq x cl:nil) (JSONBool False))
                            ((cl:numberp x) (JSONNumber (cl:coerce x 'cl:double-float)))
                            ((cl:stringp x) (JSONString x))
                            ((cl:listp x)
                             (cl:if (cl:and (cl:consp (cl:first x))
                                            (cl:stringp (cl:car (cl:first x))))
                                    ;; It's an object (alist)
                                    (JSONObject
                                     (cl:mapcar (cl:lambda (pair)
                                                  (Tuple (cl:car pair)
                                                         (convert-obj (cl:cdr pair))))
                                                x))
                                    ;; It's an array (list)
                                    (JSONArray
                                     (cl:mapcar #'convert-obj x))))
                            (cl:t (JSONString (cl:format cl:nil "~A" x))))))
              (Ok (convert-obj obj))))
        (cl:error (e)
          (Err (ParseError (cl:format cl:nil "JSON parse error: ~A" e)))))))

  ;; Core parsing function using st-json
  (declare parse-json (String -> (Result JSONError JSONValue)))
  (define (parse-json json-str)
    "Parse a JSON string into a JSONValue using st-json"
    (st-json-to-coalton-value json-str))

  ;; Simplified stringify function - just for demonstration
  (declare stringify-json (JSONValue -> String))
  (define (stringify-json json-val)
    "Convert JSONValue to JSON string (simplified version)"
    (match json-val
      ((JSONNull) "null")
      ((JSONBool True) "true") 
      ((JSONBool False) "false")
      ((JSONNumber n) (lisp String (n) (cl:format cl:nil "~A" n)))
      ((JSONString s) (lisp String (s) (cl:format cl:nil "~S" s)))
      (_ "\"[Complex JSON structure]\"")))  ;; Simplified for now

  ;; Constructor helpers
  (declare json-object ((List (Tuple String JSONValue)) -> JSONValue))
  (define (json-object pairs)
    "Create JSON object from pairs"
    (JSONObject pairs))

  (declare json-array ((List JSONValue) -> JSONValue))
  (define (json-array items)
    "Create JSON array from items"
    (JSONArray items))

  ;; TODO: Complex functions like json-get, json-get-string, etc. will be added
  ;; once the basic parsing functionality is working
  )