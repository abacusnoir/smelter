;;;; smelter-json.lisp - JSON adapter for Smelter (working version)

(cl:defpackage #:smelter/json
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:result #:coalton-library/result))
  (:export
   #:JsonValue
   #:JsonNull
   #:JsonBool
   #:JsonNumber
   #:JsonString
   #:JsonArray
   #:JsonObject
   
   #:JsonError
   #:ParseError
   #:TypeError
   
   #:parse-json
   #:stringify-json))

(cl:in-package #:smelter/json)

(coalton-toplevel
  (define-type JsonValue
    JsonNull
    (JsonBool Boolean)
    (JsonNumber Integer)
    (JsonString String)
    (JsonArray (List JsonValue))
    (JsonObject (List (Tuple String JsonValue))))
  
  (define-type JsonError
    (ParseError String)
    (TypeError String)))

(cl:defun %parse-json-simple (json-str)
  "Simple JSON parse - primitives only for now"
  (cl:handler-case
      (cl:let ((parsed (st-json:read-json json-str)))
        (cl:cond
          ((cl:null parsed) (cl:list :ok :null))
          ((cl:eq parsed :true) (cl:list :ok :bool cl:t))
          ((cl:eq parsed :false) (cl:list :ok :bool cl:nil))
          ((cl:numberp parsed) (cl:list :ok :number (cl:truncate parsed)))
          ((cl:stringp parsed) (cl:list :ok :string parsed))
          (cl:t (cl:list :ok :string (cl:format cl:nil "~A" parsed)))))
    (cl:error (e)
      (cl:list :error (cl:format cl:nil "~A" e)))))

(coalton-toplevel
  (declare parse-json (String -> (Result JsonError JsonValue)))
  (define (parse-json json-str)
    (lisp (Result JsonError JsonValue) (json-str)
      (cl:let ((result (%parse-json-simple json-str)))
        (cl:case (cl:first result)
          (:ok
           (cl:case (cl:second result)
             (:null (Ok JsonNull))
             (:bool (Ok (JsonBool (cl:third result))))
             (:number (Ok (JsonNumber (cl:third result))))
             (:string (Ok (JsonString (cl:third result))))
             (cl:t (Ok JsonNull))))
          (:error (Err (ParseError (cl:second result))))
          (cl:t (Err (ParseError "Unknown parse error")))))))

  (declare stringify-json (JsonValue -> String))
  (define (stringify-json json-val)
    (match json-val
      ((JsonNull) "null")
      ((JsonBool b) (if b "true" "false"))
      ((JsonNumber n) (into n))
      ((JsonString s) 
       (lisp String (s)
         (cl:format cl:nil "~S" s)))
      ((JsonArray _) "[]")
      ((JsonObject _) "{}"))))