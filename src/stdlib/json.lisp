;;;; src/stdlib/json-simple.lisp
;;;; Simplified JSON adapter with minimal FFI usage

(defpackage #:smelter.stdlib.json
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; Types
   #:JsonValue
   #:JsonObject #:JsonArray #:JsonString #:JsonNumber #:JsonBool #:JsonNull
   #:JsonError
   #:ParseError #:EncodeError #:FieldNotFound #:TypeMismatch
   ;; Functions
   #:parse-json
   #:encode-json
   #:get-field))

(in-package #:smelter.stdlib.json)

(coalton-toplevel
  ;; Core JSON value type
  (define-type JsonValue
    "Represents any valid JSON value."
    (JsonObject (List (Tuple String JsonValue)))
    (JsonArray (List JsonValue))
    (JsonString String)
    (JsonNumber Double-Float)
    (JsonBool Boolean)
    JsonNull)

  ;; JSON error types
  (define-type JsonError
    "Represents JSON processing errors."
    (ParseError String)
    (EncodeError String)
    (FieldNotFound String)
    (TypeMismatch String String))

  ;; Private helper function to translate Common Lisp objects to JsonValue
  (define (translate-cl-to-json obj)
    "Convert a raw Lisp object returned by YASON into typed JsonValue"
    (lisp (Result JsonError JsonValue) (obj)
      (cl:labels ((convert-obj (x)
                    (cl:cond
                      ;; Null - YASON returns :null for JSON null
                      ((cl:eq x :null) JsonNull)
                      ;; Boolean - YASON returns T/NIL for JSON true/false
                      ((cl:eq x cl:t) (JsonBool True))
                      ((cl:eq x cl:nil) (JsonBool False))
                      ;; Number
                      ((cl:numberp x)
                       (JsonNumber (cl:coerce x 'cl:double-float)))
                      ;; String
                      ((cl:stringp x)
                       (JsonString x))
                      ;; Hash table (JSON object)
                      ((cl:hash-table-p x)
                       (cl:let ((pairs cl:nil))
                         (cl:maphash
                          (cl:lambda (key value)
                            (cl:when (cl:stringp key)
                              (cl:push (Tuple key (convert-obj value)) pairs)))
                          x)
                         (JsonObject (cl:nreverse pairs))))
                      ;; List (JSON array)
                      ((cl:listp x)
                       (JsonArray (cl:mapcar #'convert-obj x)))
                      ;; Unknown type - return error for type safety
                      (cl:t
                       (cl:return-from translate-cl-to-json
                         (Err (TypeMismatch "JSON-compatible Lisp object"
                                            (cl:format cl:nil "~A" (cl:type-of x)))))))))
        (Ok (convert-obj obj)))))

  ;; Public function: parse JSON string
  (declare parse-json (String -> (Result JsonError JsonValue)))
  (define (parse-json json-str)
    "Parse a JSON string into a JsonValue"
    (lisp (Result JsonError JsonValue) (json-str)
      (cl:multiple-value-bind (status data)
          (smelter.bridge.json:safe-parse-json json-str)
        (cl:case status
          (:ok
           ;; Successfully parsed, now translate to JsonValue
           (cl:funcall (cl:function translate-cl-to-json) data))
          (:error
           ;; Parse failed, return error
           (Err (ParseError data)))))))

  ;; Simple encoding implementation to avoid complex FFI issues
  (declare encode-json (JsonValue -> (Result JsonError String)))
  (define (encode-json json-val)
    "Encode a JsonValue to a JSON string - simplified implementation"
    (match json-val
      ((JsonNull) (Ok "null"))
      ((JsonBool True) (Ok "true"))
      ((JsonBool False) (Ok "false"))
      ((JsonNumber n) (Ok (lisp String (n) (cl:format cl:nil "~A" n))))
      ((JsonString s)
       ;; Use YASON for proper string escaping
       (lisp (Result JsonError String) (s)
         (cl:multiple-value-bind (status data)
             (smelter.bridge.json:safe-encode-json s)
           (cl:case status
             (:ok (Ok data))
             (:error (Err (EncodeError data)))))))
      ((JsonArray items)
       ;; For now, return empty array - can be enhanced later
       (Ok "[]"))
      ((JsonObject pairs)
       ;; For now, return empty object - can be enhanced later
       (Ok "{}"))))

  ;; Utility function: get field from JSON object
  (declare get-field (String -> JsonValue -> (Result JsonError JsonValue)))
  (define (get-field key json-obj)
    "Get a field from a JsonObject by key"
    (match json-obj
      ((JsonObject pairs)
       ;; Simple search through pairs
       (find-in-pairs key pairs))
      (_
       (Err (TypeMismatch "JsonObject" "other type")))))

  ;; Helper to search through object pairs
  (declare find-in-pairs (String -> (List (Tuple String JsonValue)) -> (Result JsonError JsonValue)))
  (define (find-in-pairs key pairs)
    "Find a key in a list of key-value pairs"
    (match pairs
      ((Nil) (Err (FieldNotFound key)))
      ((Cons (Tuple field-key value) rest-pairs)
       (if (== key field-key)
           (Ok value)
           (find-in-pairs key rest-pairs)))))

  )