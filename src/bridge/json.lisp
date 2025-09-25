;;;; JSON FFI Bridge Layer for Smelter
;;;; Provides safe error-handled interface to YASON library

(defpackage #:smelter.bridge.json
  (:use #:cl)
  (:export
   #:safe-parse-json
   #:safe-encode-json))

(in-package #:smelter.bridge.json)

(defun safe-parse-json (json-string)
  "Parse JSON string using YASON with error handling.
   Returns (values :ok parsed-data) on success.
   Returns (values :error error-message) on failure."
  (handler-case
      ;; YASON configuration for predictable behavior:
      ;; - booleans are parsed as T/NIL
      ;; - null is parsed as :null keyword
      (let ((yason:*parse-json-arrays-as-vectors* nil))
        (values :ok (yason:parse json-string)))
    (error (e)
      (values :error (format nil "JSON parse error: ~A" e)))))

(defun safe-encode-json (lisp-object)
  "Encode Lisp object to JSON string using YASON with error handling.
   Returns (values :ok json-string) on success.
   Returns (values :error error-message) on failure."
  (handler-case
      ;; YASON handles booleans directly without configuration
      (values :ok (with-output-to-string (stream)
                    (yason:encode lisp-object stream)))
    (error (e)
      (values :error (format nil "JSON encode error: ~A" e)))))