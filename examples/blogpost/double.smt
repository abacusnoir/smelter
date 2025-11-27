#!/usr/bin/env smt run

;; A pure function: same input always gives same output
(declare double (Integer -> Integer))
(define (double x)
  (* 2 x))

(define main
  (progn
    (println "double 21 =>")
    (println (show (double 21)))
    (println "double 100 =>")
    (println (show (double 100)))))
