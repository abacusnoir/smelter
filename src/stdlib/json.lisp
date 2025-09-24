;;;; src/stdlib/json.lisp
;;;; Type-safe Coalton JSON adapter layer for Smelter

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
    (TypeMismatch String String))  ; expected, actual

  ;; Private helper function to translate Common Lisp objects to JsonValue
  (define (translate-cl-to-json obj)
    "Convert a raw Lisp object returned by YASON into typed JsonValue"
    (lisp (Result JsonError JsonValue) (obj)
      (cl:labels ((convert-obj (x)
                    (cl:cond
                      ;; Null - handled by explicit bridge configuration
                      ((cl:eq x :null) JsonNull)
                      ;; Boolean - handled by explicit bridge configuration
                      ((cl:eq x :true) (JsonBool True))
                      ((cl:eq x :false) (JsonBool False))
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

  ;; Private helper function to translate JsonValue to Common Lisp objects
  (define (translate-json-to-cl json-val)
    "Convert JsonValue to Lisp object that YASON can encode"
    (lisp lisp:object (json-val)
      (cl:labels ((convert-json (val)
                    (match val
                      ((JsonNull) :null)
                      ((JsonBool True) :true)
                      ((JsonBool False) :false)
                      ((JsonNumber n) n)
                      ((JsonString s) s)
                      ((JsonArray items)
                       (cl:mapcar #'convert-json items))
                      ((JsonObject pairs)
                       (cl:let ((hash (cl:make-hash-table :test #'cl:equal)))
                         (cl:dolist (pair pairs)
                           (match pair
                             ((Tuple key value)
                              (cl:setf (cl:gethash key hash)
                                       (convert-json value)))))
                         hash)))))
        (convert-json json-val))))

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

  ;; Public function: encode JsonValue to JSON string
  (declare encode-json (JsonValue -> (Result JsonError String)))
  (define (encode-json json-val)
    "Encode a JsonValue to a JSON string"
    (lisp (Result JsonError String) (json-val)
      (cl:let ((cl-obj (cl:funcall (cl:function translate-json-to-cl) json-val)))
        (cl:multiple-value-bind (status data)
            (smelter.bridge.json:safe-encode-json cl-obj)
          (cl:case status
            (:ok
             (Ok data))
            (:error
             (Err (EncodeError data))))))))

  ;; Utility function: get field from JSON object
  (declare get-field (String -> JsonValue -> (Result JsonError JsonValue)))
  (define (get-field key json-obj)
    "Get a field from a JsonObject by key"
    (match json-obj
      ((JsonObject pairs)
       (lisp (Result JsonError JsonValue) (key pairs)
         (cl:block field-search
           (cl:dolist (pair pairs)
             (match pair
               ((Tuple field-key value)
                (cl:when (cl:string= key field-key)
                  (cl:return-from field-search (Ok value))))))
           (Err (FieldNotFound key)))))
      (_
       (Err (TypeMismatch "JsonObject" "other type")))))

  )