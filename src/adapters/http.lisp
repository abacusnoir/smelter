(defpackage #:smelter/adapters/http
  (:use #:coalton #:coalton-prelude)
  (:export
   #:Method #:GET #:POST #:PUT #:DELETE #:PATCH
   #:Headers 
   #:Request #:Response
   #:HttpError #:NetworkError #:TimeoutError #:ParseError
   #:http-request
   #:http-get
   #:http-post
   #:http-get-json
   #:make-headers
   #:response-status
   #:response-body
   #:response-headers
   #:get-header))

(in-package #:smelter/adapters/http)

(coalton-toplevel
  (define-type Method
    GET
    POST  
    PUT
    DELETE
    PATCH)

  (define-type Headers
    (Headers (List (Tuple String String))))

  (define-type Request
    (Request String Method Headers (Optional String)))

  (define-type Response
    (Response Integer Headers String))

  (define-type HttpError
    (NetworkError String)
    (TimeoutError String)
    (ParseError String))

  (define (alist-to-headers alist)
    "Convert alist to Headers"
    (Headers
     (lisp (List (Tuple String String)) (alist)
       (cl:mapcar (cl:lambda (pair)
                    (Tuple (cl:car pair) (cl:cdr pair)))
                  alist))))

  (declare make-headers ((List (Tuple String String)) -> Headers))
  (define (make-headers pairs)
    "Create Headers from list of pairs"
    (Headers pairs))

  (declare response-status (Response -> Integer))
  (define (response-status response)
    "Get status code from response"
    (match response
      ((Response status _ _) status)))

  (declare response-headers (Response -> Headers))
  (define (response-headers response)
    "Get headers from response"
    (match response
      ((Response _ headers _) headers)))

  (declare response-body (Response -> String))
  (define (response-body response)
    "Get body from response"
    (match response
      ((Response _ _ body) body)))

  (declare get-header (String -> Headers -> (Optional String)))
  (define (get-header name headers)
    "Get header value by name (case-insensitive)"
    (match headers
      ((Headers pairs)
       (find-header-value (to-lowercase name) pairs))))

  (declare find-header-value (String -> (List (Tuple String String)) -> (Optional String)))
  (define (find-header-value name pairs)
    "Find header value in pairs list"
    (match pairs
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (== (to-lowercase k) name)
           (Some v)
           (find-header-value name rest)))))

  (declare to-lowercase (String -> String))
  (define (to-lowercase s)
    "Convert string to lowercase"
    (lisp String (s)
      (cl:string-downcase s)))

  ;; Simplified HTTP GET using drakma directly
  (declare http-get (String -> (Result HttpError Response)))
  (define (http-get url)
    "Make GET request"
    (lisp (Result HttpError Response) (url)
      (cl:handler-case
          (cl:multiple-value-bind (body status headers)
              (drakma:http-request url :method :get :want-stream cl:nil)
            (cl:let ((body-str (cl:if (cl:stringp body)
                                      body
                                      (cl:if (cl:and body (cl:typep body '(cl:vector cl:unsigned-byte)))
                                             (flexi-streams:octets-to-string body :external-format :utf-8)
                                             (cl:format cl:nil "~A" body)))))
              (Ok (Response status
                            (alist-to-headers headers)
                            body-str))))
        (cl:error (e)
          (Err (NetworkError (cl:format cl:nil "HTTP GET failed: ~A" e)))))))

  (declare http-post (String -> String -> (Result HttpError Response)))
  (define (http-post url post-body)
    "Make POST request with body"
    (lisp (Result HttpError Response) (url post-body)
      (cl:handler-case
          (cl:multiple-value-bind (body status headers)
              (drakma:http-request url
                                   :method :post
                                   :content post-body
                                   :content-type "application/json"
                                   :want-stream cl:nil)
            (cl:let ((body-str (cl:if (cl:stringp body)
                                      body
                                      (cl:if (cl:and body (cl:typep body '(cl:vector cl:unsigned-byte)))
                                             (flexi-streams:octets-to-string body :external-format :utf-8)
                                             (cl:format cl:nil "~A" body)))))
              (Ok (Response status
                            (alist-to-headers headers)
                            body-str))))
        (cl:error (e)
          (Err (NetworkError (cl:format cl:nil "HTTP POST failed: ~A" e)))))))

  (declare http-get-json (String -> (Result HttpError Response)))
  (define (http-get-json url)
    "Make GET request expecting JSON response"
    (lisp (Result HttpError Response) (url)
      (cl:handler-case
          (cl:multiple-value-bind (body status headers)
              (drakma:http-request url
                                   :method :get
                                   :additional-headers '(("Accept" . "application/json"))
                                   :want-stream cl:nil)
            (cl:let ((body-str (cl:if (cl:stringp body)
                                      body
                                      (cl:if (cl:and body (cl:typep body '(cl:vector cl:unsigned-byte)))
                                             (flexi-streams:octets-to-string body :external-format :utf-8)
                                             (cl:format cl:nil "~A" body)))))
              (Ok (Response status
                            (alist-to-headers headers)
                            body-str))))
        (cl:error (e)
          (Err (NetworkError (cl:format cl:nil "HTTP GET JSON failed: ~A" e)))))))

  ;; Generic http-request still needed for compatibility
  (declare http-request (Request -> (Result HttpError Response)))
  (define (http-request req)
    "Generic HTTP request - delegates to specific methods"
    (match req
      ((Request url GET _ _) (http-get url))
      ((Request url POST _ (Some body)) (http-post url body))
      ((Request url POST _ None) (http-post url ""))
      (_ (Err (NetworkError "Unsupported HTTP method"))))))