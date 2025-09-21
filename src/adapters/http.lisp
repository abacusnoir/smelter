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
    "Convert Headers to Common Lisp alist for drakma"
    (match headers
      ((Headers pairs)
       (lisp cl:list (pairs)
         (cl:mapcar (cl:lambda (pair)
                      (cl:cons (fst pair) (snd pair)))
                    pairs)))))

  (declare alist-to-headers (cl:list -> Headers))
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

  ;; Core HTTP request function using drakma
  (declare http-request (Request -> (Result HttpError Response)))
  (define (http-request request)
    "Make HTTP request using drakma"
    (match request
      ((Request url method headers body)
       (lisp (Result HttpError Response) (url method headers body)
         (cl:handler-case
             (cl:multiple-value-bind (response-body status-code response-headers)
                 (cl:apply #'drakma:http-request 
                           url
                           (cl:append
                            (cl:list :method (method-to-keyword method)
                                     :additional-headers (headers-to-alist headers)
                                     :want-stream cl:nil
                                     :timeout 30)
                            (cl:when body
                              (cl:list :content body))))
               ;; Convert response body to string
               (cl:let ((body-str (cl:if (cl:stringp response-body)
                                         response-body
                                         (cl:if (cl:and response-body
                                                        (cl:typep response-body '(cl:vector cl:unsigned-byte)))
                                                (flexi-streams:octets-to-string response-body :external-format :utf-8)
                                                (cl:format cl:nil "~A" response-body)))))
                 (Ok (Response status-code 
                               (alist-to-headers response-headers)
                               body-str))))
           (cl:error (e)
             (Err (NetworkError (cl:format cl:nil "HTTP request failed: ~A" e)))))))))

  ;; Convenience functions
  (declare http-get (String -> (Result HttpError Response)))
  (define (http-get url)
    "Make GET request"
    (http-request (Request url GET (Headers Nil) None)))

  (declare http-post (String -> String -> (Result HttpError Response)))
  (define (http-post url body)
    "Make POST request with body"
    (http-request (Request url POST 
                          (Headers (make-list (Tuple "Content-Type" "application/json")))
                          (Some body))))

  (declare http-get-json (String -> (Result HttpError Response)))
  (define (http-get-json url)
    "Make GET request expecting JSON response"
    (http-request (Request url GET 
                          (Headers (make-list (Tuple "Accept" "application/json")))
                          None))))