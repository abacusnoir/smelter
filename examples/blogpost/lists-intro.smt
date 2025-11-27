#!/usr/bin/env smt run

;; Helper to show a list
(declare show-list-helper (coalton:List Integer -> String))
(define (show-list-helper lst)
  (match lst
    ((Nil) "")
    ((Cons x xs)
     (match xs
       ((Nil) (show x))
       ((Cons _ _) (<> (show x) (<> " " (show-list-helper xs))))))))

(declare show-list (coalton:List Integer -> String))
(define (show-list lst)
  (<> "#(" (<> (show-list-helper lst) ")")))

;; Sum a list recursively
(declare sum-list (coalton:List Integer -> Integer))
(define (sum-list lst)
  (match lst
    ((Nil) 0)
    ((Cons x xs) (+ x (sum-list xs)))))

;; Double each element
(declare double-all (coalton:List Integer -> coalton:List Integer))
(define (double-all lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (* 2 x) (double-all xs)))))

(define main
  (let ((numbers (make-list 1 2 3 4 5)))
    (progn
      (println "Original list: 1 2 3 4 5")
      (println (<> "Sum: " (show (sum-list numbers))))
      (println "Doubled: ")
      (println (show-list (double-all numbers))))))
