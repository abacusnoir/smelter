#!/usr/bin/env smt run

(declare risky-describe ((Optional Integer) -> String))
(define (risky-describe opt)
  (match opt
    ((Some n) (<> "Got: " (show n)))))
    ;; Oops - forgot None!

(define main
  (println (risky-describe (Some 42))))
