#!/usr/bin/env smt run

;; Simple transformations
(declare add-one (Integer -> Integer))
(define (add-one x)
  (+ x 1))

(declare double (Integer -> Integer))
(define (double x)
  (* 2 x))

(declare square (Integer -> Integer))
(define (square x)
  (* x x))

;; Compose them!
(declare transform (Integer -> Integer))
(define (transform x)
  (square (double (add-one x))))

;; Break down the steps
(declare show-steps (Integer -> Unit))
(define (show-steps x)
  (let ((step1 (add-one x))
        (step2 (double (add-one x)))
        (step3 (square (double (add-one x)))))
    (progn
      (println (<> "Start with: " (show x)))
      (println (<> "After add-one: " (show step1)))
      (println (<> "After double: " (show step2)))
      (println (<> "After square: " (show step3))))))

(define main
  (progn
    (println "=== Direct transformation ===")
    (println (show (transform 5)))
    (println "")
    (println "=== Step by step ===")
    (show-steps 5)))
