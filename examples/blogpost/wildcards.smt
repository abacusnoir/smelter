#!/usr/bin/env smt run

(declare is-some ((Optional Integer) -> Boolean))
(define (is-some opt)
  (match opt
    ((Some _) True)
    (_ False)))

(declare show-bool (Boolean -> String))
(define (show-bool b)
  (if b "True" "False"))

(define main
  (progn
    (println (show-bool (is-some (Some 99))))
    (println (show-bool (is-some None)))))
