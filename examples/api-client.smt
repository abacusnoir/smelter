#!/usr/bin/env smt run
;; API Client Example - Demonstrates HTTP and JSON adapters

(coalton-toplevel
  (define (fetch-json-api url)
    "Fetch JSON data from an API endpoint"
    (let ((response (smelter/http:http-get url)))
      (match response
        ((Ok json-str)
         (smelter/json:parse-json json-str))
        ((Err err)
         (Err (smelter/json:ParseError 
               (match err
                 ((smelter/http:NetworkError msg) msg)
                 ((smelter/http:TimeoutError) "Request timed out")
                 ((smelter/http:HttpStatus code msg) 
                  (concat (concat "HTTP " (into code)) (concat ": " msg))))))))))

  (define (print-user-info json)
    "Extract and print user information from JSON"
    (match json
      ((smelter/json:JsonObject _)
       (let ((url-opt (smelter/json:json-get-string "url" json))
             (origin-opt (smelter/json:json-get-string "origin" json))
             (user-agent-opt (match (smelter/json:json-get "headers" json)
                               ((Some (smelter/json:JsonObject headers-list))
                                (let ((headers-json (smelter/json:JsonObject headers-list)))
                                  (smelter/json:json-get-string "User-Agent" headers-json)))
                               (_ None))))
         (progn
           (println "=== API Response ===")
           (match url-opt
             ((Some url) (println (concat "URL: " url)))
             (None (println "URL: Not found")))
           (match origin-opt
             ((Some origin) (println (concat "Your IP: " origin)))
             (None (println "Origin: Not found")))
           (match user-agent-opt
             ((Some agent) (println (concat "User-Agent: " agent)))
             (None (println "User-Agent: Not found")))
           Unit)))
      (_ (println "Error: Response is not a JSON object"))))

  (define (fetch-and-display-posts)
    "Fetch sample posts from JSONPlaceholder API"
    (let ((result (fetch-json-api "https://jsonplaceholder.typicode.com/posts/1")))
      (match result
        ((Ok json)
         (progn
           (println "\n=== Sample Post ===")
           (match (smelter/json:json-get-string "title" json)
             ((Some title) (println (concat "Title: " title)))
             (None (println "No title found")))
           (match (smelter/json:json-get-string "body" json)
             ((Some body) (println (concat "Body: " body)))
             (None (println "No body found")))
           (match (smelter/json:json-get-number "userId" json)
             ((Some user-id) (println (concat "User ID: " (into user-id))))
             (None (println "No user ID found")))
           Unit))
        ((Err err)
         (match err
           ((smelter/json:ParseError msg)
            (println (concat "Error fetching post: " msg)))
           ((smelter/json:TypeError msg)
            (println (concat "Type error: " msg))))))))

  (define (test-post-request)
    "Test POST request with JSON data"
    (let ((json-data "{\"title\":\"Test Post\",\"body\":\"This is a test\",\"userId\":1}")
          (response (smelter/http:http-post 
                     "https://jsonplaceholder.typicode.com/posts"
                     json-data)))
      (match response
        ((Ok json-str)
         (match (smelter/json:parse-json json-str)
           ((Ok json)
            (progn
              (println "\n=== POST Response ===")
              (match (smelter/json:json-get-number "id" json)
                ((Some id) (println (concat "Created post with ID: " (into id))))
                (None (println "No ID in response")))
              Unit))
           ((Err err)
            (match err
              ((smelter/json:ParseError msg)
               (println (concat "JSON parse error: " msg)))
              ((smelter/json:TypeError msg)
               (println (concat "Type error: " msg)))))))
        ((Err err)
         (match err
           ((smelter/http:NetworkError msg)
            (println (concat "Network error: " msg)))
           ((smelter/http:TimeoutError)
            (println "Request timed out"))
           ((smelter/http:HttpStatus code msg)
            (println (concat (concat "HTTP error " (into code)) 
                             (concat ": " msg)))))))))

  (define (main)
    "Main entry point"
    (progn
      (println "Smelter API Client Demo")
      (println "=======================\n")
      
      ;; Test GET request to httpbin
      (println "Fetching from httpbin.org...")
      (let ((result (fetch-json-api "https://httpbin.org/get")))
        (match result
          ((Ok json) (print-user-info json))
          ((Err err) 
           (match err
             ((smelter/json:ParseError msg)
              (println (concat "Error: " msg)))
             ((smelter/json:TypeError msg)
              (println (concat "Type error: " msg)))))))
      
      ;; Test fetching a post
      (fetch-and-display-posts)
      
      ;; Test POST request
      (test-post-request)
      
      (println "\nDemo complete!")
      Unit)))