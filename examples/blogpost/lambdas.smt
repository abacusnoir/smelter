#!/usr/bin/env smt run

(define (my-map f lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (f x) (my-map f xs)))))

;; Helper to compute sum of list for demonstrating lambda results
(define (sum-list lst)
  (match lst
    ((Nil) 0)
    ((Cons x xs) (+ x (sum-list xs)))))

(define main
  (let ((numbers (make-list 1 2 3 4)))
    (progn
      ;; Square each number with a lambda
      (println (<> "Squared (1+4+9+16): " (show (sum-list (my-map (fn (x) (* x x)) numbers)))))
      ;; Add 10 to each
      (println (<> "Plus 10 (11+12+13+14): " (show (sum-list (my-map (fn (x) (+ x 10)) numbers))))))))
