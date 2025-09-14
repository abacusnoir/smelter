#!/usr/bin/env smt run
;; Simple HTTP test

(coalton-toplevel
  (define (test-http)
    (let ((result (smelter/http:http-get "https://httpbin.org/get")))
      (match result
        ((Ok response)
         (progn
           (println "Success! Response (truncated):")
           (println (coalton-library/string:substring response 0 500))))
        ((Err err)
         (match err
           ((smelter/http:NetworkError msg)
            (println msg))
           ((smelter/http:TimeoutError)
            (println "Request timed out"))
           ((smelter/http:HttpStatus code msg)
            (println msg)))))))
  
  (define (main)
    (progn
      (println "Testing HTTP GET request...")
      (test-http)
      Unit)))