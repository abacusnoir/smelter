#!/usr/bin/env smt run

(define (my-length lst)
  (match lst
    ((Nil) 0)
    ((Cons _ rest) (+ 1 (my-length rest)))))

(define (my-sum lst)
  (match lst
    ((Nil) 0)
    ((Cons x rest) (+ x (my-sum rest)))))

(define main
  (progn
    (println (<> "Length: " (show (my-length (make-list 1 2 3 4)))))
    (println (<> "Sum: " (show (my-sum (make-list 1 2 3 4)))))))
