#!/usr/bin/env smt run

;; Apply a function twice to a value
(declare apply-twice ((:a -> :a) -> :a -> :a))
(define (apply-twice f x)
  (f (f x)))

;; A simple function to pass around
(declare add-three (Integer -> Integer))
(define (add-three n)
  (+ n 3))

(define main
  (progn
    (println (<> "apply-twice add-three 10 = " (show (apply-twice add-three 10))))
    (println (<> "apply-twice add-three 0 = " (show (apply-twice add-three 0))))))
