#!/usr/bin/env smt run

;; Our own map implementation (type inference handles polymorphism)
(define (my-map f lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (f x) (my-map f xs)))))

;; Our own filter implementation
(define (my-filter pred lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs)
      (if (pred x)
          (Cons x (my-filter pred xs))
          (my-filter pred xs)))))

;; Our own fold-right implementation
(define (my-foldr f init lst)
  (match lst
    ((Nil) init)
    ((Cons x xs) (f x (my-foldr f init xs)))))

;; Helper predicates
(declare is-even (Integer -> Boolean))
(define (is-even n)
  (== (mod n 2) 0))

(declare double (Integer -> Integer))
(define (double n)
  (* 2 n))

;; Helper to show list contents (sum + count)
(define (sum-list lst)
  (my-foldr + 0 lst))

(define (count-list lst)
  (my-foldr (fn (_x acc) (+ 1 acc)) 0 lst))

(define main
  (let ((numbers (make-list 1 2 3 4 5)))
    (progn
      (println (<> "Original sum (1+2+3+4+5): " (show (sum-list numbers))))
      (println (<> "Doubled sum (2+4+6+8+10): " (show (sum-list (my-map double numbers)))))
      (println (<> "Evens only sum (2+4): " (show (sum-list (my-filter is-even numbers)))))
      (println (<> "Evens only count: " (show (count-list (my-filter is-even numbers))))))))
