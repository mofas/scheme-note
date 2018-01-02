#lang racket
(require "./ch3_constraint.rkt")


(define (averager a b c)
  (let [(u (make-connector))
        (v (make-connector))]
  (adder a b u) ;; a + b = u
  (constant 2 v) ;; v = 2
  (multiplier v c u)) ;; (a + b) = 2 * c
  'ok)


