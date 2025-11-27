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

;; TODO: Implement reverse-list
;; Hint: Use an accumulator for efficiency
(declare reverse-list (coalton:List Integer -> coalton:List Integer))
(define (reverse-list lst)
  ;; Your implementation here
  ;; Try both naive recursion and accumulator style
  Nil)  ;; Replace this

;; Test helper
(declare list-equal (coalton:List Integer -> coalton:List Integer -> Boolean))
(define (list-equal lst1 lst2)
  (match lst1
    ((Nil)
     (match lst2
       ((Nil) True)
       ((Cons _ _) False)))
    ((Cons x xs)
     (match lst2
       ((Nil) False)
       ((Cons y ys) (and (== x y) (list-equal xs ys)))))))

(define main
  (let ((original (make-list 1 2 3 4 5))
        (reversed (reverse-list original))
        (expected (make-list 5 4 3 2 1)))
    (progn
      (println (<> "Original: " (show-list original)))
      (println (<> "Reversed: " (show-list reversed)))
      (println (<> "Correct? "
                  (if (list-equal reversed expected)
                      "yes"
                      "no"))))))
