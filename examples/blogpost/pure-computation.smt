#!/usr/bin/env smt run

;; Pure computation: just transforms data
(declare process (Integer -> Integer))
(define (process x)
  (* 2 x))

;; Pure computation: checks if even
(declare is-even (Integer -> Boolean))
(define (is-even n)
  (== (mod n 2) 0))

;; Helper to show boolean
(declare show-bool (Boolean -> String))
(define (show-bool b)
  (if b "yes" "no"))

;; Main handles ALL the I/O in one place
(define main
  (progn
    (println "Processing numbers...")
    (let ((result (process 21)))
      (progn
        (println (show result))
        (println (<> "Is " (<> (show result) (<> " even? " (show-bool (is-even result))))))))))
