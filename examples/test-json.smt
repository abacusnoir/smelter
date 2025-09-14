#!/usr/bin/env smt run
;; Simple JSON test

(coalton-toplevel
  (define (main)
    (let ((json-str "{\"name\":\"test\",\"value\":42}")
          (result (smelter/json:parse-json json-str)))
      (match result
        ((Ok json)
         (progn
           (println "Parsed JSON:")
           (println (smelter/json:stringify-json json))
           Unit))
        ((Err err)
         (match err
           ((smelter/json:ParseError msg)
            (println msg))
           ((smelter/json:TypeError msg)
            (println msg))))))))