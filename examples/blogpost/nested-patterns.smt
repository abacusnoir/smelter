#!/usr/bin/env smt run

(declare describe-pair ((Tuple (Optional Integer) (Optional Integer)) -> String))
(define (describe-pair pair)
  (match pair
    ((Tuple (None) (None)) "Both empty")
    ((Tuple (Some a) (None)) (<> "First: " (show a)))
    ((Tuple (None) (Some b)) (<> "Second: " (show b)))
    ((Tuple (Some a) (Some b)) (<> "Both: " (<> (show a) (<> " and " (show b)))))))

(define main
  (progn
    (println (describe-pair (Tuple None None)))
    (println (describe-pair (Tuple (Some 1) None)))
    (println (describe-pair (Tuple None (Some 2))))
    (println (describe-pair (Tuple (Some 1) (Some 2))))))
