;;;; smelter-http.lisp - HTTP adapter for Smelter

(cl:defpackage #:smelter/http
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:result #:coalton-library/result))
  (:export
   #:HttpError
   #:NetworkError
   #:TimeoutError
   #:HttpStatus
   
   #:http-get
   #:http-post
   #:http-put
   #:http-delete))

(cl:in-package #:smelter/http)

(coalton-toplevel
  (define-type HttpError
    (NetworkError String)
    (TimeoutError)
    (HttpStatus Integer String)))

(cl:defun %http-get (url)
  "Internal HTTP GET request"
  (cl:handler-case
      (cl:multiple-value-bind (body-str status-code headers uri stream must-close reason-phrase)
          (drakma:http-request url
                               :method :get
                               :want-stream cl:nil)
        (cl:declare (cl:ignore headers uri stream must-close))
        (cl:if (cl:and (cl:>= status-code 200) (cl:< status-code 300))
               (cl:list :ok (cl:if (cl:stringp body-str)
                                    body-str
                                    (flexi-streams:octets-to-string body-str)))
               (cl:list :error status-code (cl:or reason-phrase "HTTP Error"))))
    (drakma:drakma-error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))
    (cl:error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))))

(cl:defun %http-post (url body)
  "Internal HTTP POST request"
  (cl:handler-case
      (cl:multiple-value-bind (body-str status-code headers uri stream must-close reason-phrase)
          (drakma:http-request url
                               :method :post
                               :content body
                               :want-stream cl:nil)
        (cl:declare (cl:ignore headers uri stream must-close))
        (cl:if (cl:and (cl:>= status-code 200) (cl:< status-code 300))
               (cl:list :ok (cl:if (cl:stringp body-str)
                                    body-str
                                    (flexi-streams:octets-to-string body-str)))
               (cl:list :error status-code (cl:or reason-phrase "HTTP Error"))))
    (drakma:drakma-error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))
    (cl:error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))))

(cl:defun %http-put (url body)
  "Internal HTTP PUT request"
  (cl:handler-case
      (cl:multiple-value-bind (body-str status-code headers uri stream must-close reason-phrase)
          (drakma:http-request url
                               :method :put
                               :content body
                               :want-stream cl:nil)
        (cl:declare (cl:ignore headers uri stream must-close))
        (cl:if (cl:and (cl:>= status-code 200) (cl:< status-code 300))
               (cl:list :ok (cl:if (cl:stringp body-str)
                                    body-str
                                    (flexi-streams:octets-to-string body-str)))
               (cl:list :error status-code (cl:or reason-phrase "HTTP Error"))))
    (drakma:drakma-error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))
    (cl:error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))))

(cl:defun %http-delete (url)
  "Internal HTTP DELETE request"
  (cl:handler-case
      (cl:multiple-value-bind (body-str status-code headers uri stream must-close reason-phrase)
          (drakma:http-request url
                               :method :delete
                               :want-stream cl:nil)
        (cl:declare (cl:ignore headers uri stream must-close))
        (cl:if (cl:and (cl:>= status-code 200) (cl:< status-code 300))
               (cl:list :ok (cl:if (cl:stringp body-str)
                                    body-str
                                    (flexi-streams:octets-to-string body-str)))
               (cl:list :error status-code (cl:or reason-phrase "HTTP Error"))))
    (drakma:drakma-error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))
    (cl:error (e)
      (cl:list :network-error (cl:format cl:nil "~A" e)))))

(coalton-toplevel
  (declare http-get (String -> (Result HttpError String)))
  (define (http-get url)
    (lisp (Result HttpError String) (url)
      (cl:let ((result (%http-get url)))
        (cl:case (cl:first result)
          (:ok (Ok (cl:second result)))
          (:error (Err (HttpStatus (cl:second result) (cl:third result))))
          (:network-error (Err (NetworkError (cl:second result))))
          (cl:t (Err (NetworkError "Unknown error")))))))

  (declare http-post (String -> String -> (Result HttpError String)))
  (define (http-post url body)
    (lisp (Result HttpError String) (url body)
      (cl:let ((result (%http-post url body)))
        (cl:case (cl:first result)
          (:ok (Ok (cl:second result)))
          (:error (Err (HttpStatus (cl:second result) (cl:third result))))
          (:network-error (Err (NetworkError (cl:second result))))
          (cl:t (Err (NetworkError "Unknown error")))))))

  (declare http-put (String -> String -> (Result HttpError String)))
  (define (http-put url body)
    (lisp (Result HttpError String) (url body)
      (cl:let ((result (%http-put url body)))
        (cl:case (cl:first result)
          (:ok (Ok (cl:second result)))
          (:error (Err (HttpStatus (cl:second result) (cl:third result))))
          (:network-error (Err (NetworkError (cl:second result))))
          (cl:t (Err (NetworkError "Unknown error")))))))

  (declare http-delete (String -> (Result HttpError String)))
  (define (http-delete url)
    (lisp (Result HttpError String) (url)
      (cl:let ((result (%http-delete url)))
        (cl:case (cl:first result)
          (:ok (Ok (cl:second result)))
          (:error (Err (HttpStatus (cl:second result) (cl:third result))))
          (:network-error (Err (NetworkError (cl:second result))))
          (cl:t (Err (NetworkError "Unknown error"))))))))
