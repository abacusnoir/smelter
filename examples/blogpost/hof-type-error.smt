#!/usr/bin/env smt run

(define (my-map f lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (f x) (my-map f xs)))))

;; This returns a String, not an Integer
(declare stringify (Integer -> String))
(define (stringify n)
  (show n))

;; Helper to get first element (returns the value, not Optional)
(define (first-elem lst)
  (match lst
    ((Nil) 0)
    ((Cons x _xs) x)))

(define main
  ;; Trying to use map result as List Integer when it's List String
  ;; The first-elem will return a String, but + expects Integer
  (println (show (+ 1 (first-elem (my-map stringify (make-list 1 2 3)))))))
