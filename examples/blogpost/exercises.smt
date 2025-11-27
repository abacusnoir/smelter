#!/usr/bin/env smt run

;; Exercise 1: Write a function that triples a number
(declare triple (Integer -> Integer))
(define (triple x)
  ;; Your implementation here
  (* 3 x))

;; Exercise 2: Write a function that subtracts 10
(declare subtract-ten (Integer -> Integer))
(define (subtract-ten x)
  ;; Your implementation here
  (- x 10))

;; Exercise 3: Compose them - triple first, then subtract 10
(declare triple-then-subtract (Integer -> Integer))
(define (triple-then-subtract x)
  ;; Your implementation here
  (subtract-ten (triple x)))

(define main
  (progn
    (println "triple 7 =>")
    (println (show (triple 7)))
    (println "subtract-ten 20 =>")
    (println (show (subtract-ten 20)))
    (println "triple-then-subtract 10 =>")
    (println (show (triple-then-subtract 10)))))
