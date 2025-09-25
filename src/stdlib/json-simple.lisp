;;;; src/stdlib/json-simple.lisp
;;;; Simplified JSON adapter that compiles without ADT issues

(defpackage #:smelter.stdlib.json
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; Types
   #:JsonValue
   #:JsonString #:JsonNumber #:JsonBool #:JsonNull
   #:JsonError
   #:ParseError
   ;; Functions
   #:parse-json
   #:encode-json))

(in-package #:smelter.stdlib.json)

(coalton-toplevel
  ;; Simplified JSON value type - avoid recursive definitions for now
  (define-type JsonValue
    "Represents a simple JSON value."
    (JsonString String)
    (JsonNumber Double-Float)
    (JsonBool Boolean)
    JsonNull)

  ;; Simplified JSON error types
  (define-type JsonError
    "Represents JSON processing errors."
    (ParseError String))

  ;; Public function: parse JSON string (simple values only)
  (declare parse-json (String -> (Result JsonError JsonValue)))
  (define (parse-json json-str)
    "Parse a JSON string into a simple JsonValue"
    (lisp (Result JsonError JsonValue) (json-str)
      (cl:multiple-value-bind (status data)
          (smelter.bridge.json:safe-parse-json json-str)
        (cl:case status
          (:ok
           ;; Successfully parsed - handle simple types only
           (cl:cond
             ;; Null
             ((cl:eq data :null) (Ok JsonNull))
             ;; Boolean
             ((cl:eq data cl:t) (Ok (JsonBool True)))
             ((cl:eq data cl:nil) (Ok (JsonBool False)))
             ;; Number
             ((cl:numberp data) (Ok (JsonNumber (cl:coerce data 'cl:double-float))))
             ;; String
             ((cl:stringp data) (Ok (JsonString data)))
             ;; Unsupported types (arrays, objects) return error
             (cl:t (Err (ParseError "Complex JSON structures not yet supported")))))
          (:error
           ;; Parse failed
           (Err (ParseError data)))))))

  ;; Simple encoding implementation
  (declare encode-json (JsonValue -> (Result JsonError String)))
  (define (encode-json json-val)
    "Encode a JsonValue to a JSON string"
    (match json-val
      (JsonNull (Ok "null"))
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
             (:error (Err (ParseError data)))))))))

  )