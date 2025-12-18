#!/usr/bin/env smt run

;; Describe an Optional value
(declare describe-optional ((Optional Integer) -> String))
(define (describe-optional opt)
  (match opt
    ((None) "Nothing here")
    ((Some n) (<> "Got: " (show n)))))

(define main
  (progn
    (println (describe-optional None))
    (println (describe-optional (Some 42)))))
