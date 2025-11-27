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

;; Filter elements that satisfy a predicate
(declare filter-list ((Integer -> Boolean) -> coalton:List Integer -> coalton:List Integer))
(define (filter-list pred lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs)
     (if (pred x)
         (Cons x (filter-list pred xs))
         (filter-list pred xs)))))

;; Map a function over a list
(declare map-list ((Integer -> Integer) -> coalton:List Integer -> coalton:List Integer))
(define (map-list f lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (f x) (map-list f xs)))))

;; Fold (reduce) a list from the left
(declare fold-left ((Integer -> Integer -> Integer) -> Integer -> coalton:List Integer -> Integer))
(define (fold-left f acc lst)
  (match lst
    ((Nil) acc)
    ((Cons x xs) (fold-left f (f acc x) xs))))

;; Helper predicates and functions
(declare is-even (Integer -> Boolean))
(define (is-even n)
  (== (mod n 2) 0))

(declare triple (Integer -> Integer))
(define (triple x)
  (* 3 x))

(define main
  (let ((numbers (make-list 1 2 3 4 5 6 7 8 9 10))
        (evens (filter-list is-even numbers))
        (tripled (map-list triple evens))
        (sum (fold-left + 0 tripled)))
    (progn
      (println "Starting with: 1..10")
      (println (<> "After filtering evens: " (show-list evens)))
      (println (<> "After tripling: " (show-list tripled)))
      (println (<> "Sum of result: " (show sum))))))
