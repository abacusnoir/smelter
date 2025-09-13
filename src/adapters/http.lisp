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

  (declare method-to-keyword (Method -> cl:keyword))
  (define (method-to-keyword method)
    "Convert Method to Common Lisp keyword for drakma"
    (match method
      ((GET) (lisp cl:keyword () :get))
      ((POST) (lisp cl:keyword () :post))
      ((PUT) (lisp cl:keyword () :put))
      ((DELETE) (lisp cl:keyword () :delete))
      ((PATCH) (lisp cl:keyword () :patch))))

  (declare headers-to-alist (Headers -> cl:list))
  (define (headers-to-alist headers)
    "Convert Headers to alist for drakma"
    (match headers
      ((Headers pairs)
       (lisp cl:list (pairs)
         (cl:mapcar (cl:lambda (pair)
                      (cl:cons (coalton:fst pair) (coalton:snd pair)))
                    pairs)))))

  (declare alist-to-headers (cl:list -> Headers))
  (define (alist-to-headers alist)
    "Convert alist to Headers"
    (Headers (lisp (List (Tuple String String)) (alist)
               (cl:mapcar (cl:lambda (pair)
                            (cl:make-instance 'coalton:Tuple
                                              :fst (cl:format cl:nil "~A" (cl:car pair))
                                              :snd (cl:format cl:nil "~A" (cl:cdr pair))))
                          alist))))

  (declare http-request (Request -> (Result Response HttpError)))
  (define (http-request req)
    "Execute HTTP request with comprehensive error handling"
    (match req
      ((Request url method headers body)
       (lisp (Result Response HttpError) (url method headers body)
         (cl:handler-case
             (cl:multiple-value-bind (response-body status-code response-headers)
                 (drakma:http-request url
                                      :method (method-to-keyword method)
                                      :additional-headers (headers-to-alist headers)
                                      :content (cl:when body body)
                                      :connection-timeout 30
                                      :read-timeout 30
                                      :want-stream cl:nil)
               (cl:let ((response-str (cl:if (cl:stringp response-body)
                                             response-body
                                             (flexi-streams:octets-to-string 
                                              response-body :external-format :utf-8)))
                        (response-headers-coalton (alist-to-headers response-headers)))
                 (cl:values
                  (cl:make-instance 'coalton:Ok
                                    :ok (cl:make-instance 'Response
                                                          :integer status-code
                                                          :headers response-headers-coalton
                                                          :string response-str))
                  cl:t)))
           (drakma:drakma-error (e)
             (cl:values
              (cl:make-instance 'coalton:Err
                                :err (cl:make-instance 'NetworkError 
                                                       :string (cl:format cl:nil "Network error: ~A" e)))
              cl:t))
           (cl:error (e)
             (cl:values
              (cl:make-instance 'coalton:Err
                                :err (cl:make-instance 'NetworkError 
                                                       :string (cl:format cl:nil "HTTP error: ~A" e)))
              cl:t)))))))

  (declare http-get (String -> (Result Response HttpError)))
  (define (http-get url)
    "Simple GET request"
    (http-request (Request url GET (make-headers (make-list)) None)))

  (declare http-post (String -> String -> (Result Response HttpError)))
  (define (http-post url body)
    "Simple POST request with body"
    (http-request (Request url POST 
                           (make-headers (make-list (Tuple "Content-Type" "application/json")))
                           (Some body))))

  (declare http-get-json (String -> (Result Response HttpError)))
  (define (http-get-json url)
    "GET request expecting JSON response"
    (http-request (Request url GET
                           (make-headers (make-list (Tuple "Accept" "application/json")))
                           None)))

  (declare make-headers ((List (Tuple String String)) -> Headers))
  (define (make-headers pairs)
    "Create Headers from list of key-value pairs"
    (Headers pairs))

  (declare response-status (Response -> Integer))
  (define (response-status resp)
    "Get status code from response"
    (match resp
      ((Response status _ _) status)))

  (declare response-body (Response -> String))
  (define (response-body resp)
    "Get response body as string"
    (match resp
      ((Response _ _ body) body)))

  (declare response-headers (Response -> Headers))
  (define (response-headers resp)
    "Get response headers"
    (match resp
      ((Response _ headers _) headers)))

  (declare get-header (String -> Headers -> (Optional String)))
  (define (get-header name headers)
    "Get header value by name (case-insensitive)"
    (match headers
      ((Headers pairs) (find-header-value (string-downcase name) pairs))))

  (declare find-header-value (String -> (List (Tuple String String)) -> (Optional String)))
  (define (find-header-value name pairs)
    "Helper to find header value (case-insensitive)"
    (match pairs
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (== (string-downcase k) name)
           (Some v)
           (find-header-value name rest)))))

  (declare string-downcase (String -> String))
  (define (string-downcase s)
    "Convert string to lowercase"
    (lisp String (s) (cl:string-downcase s))))