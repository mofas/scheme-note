#lang racket


(define make-accumulator
  (λ(init)
    (λ(inc)
      (begin
        (set! init (+ init inc))
        init
        ))))


(define A (make-accumulator 5))

(A 10)
;; 15
(A 10)
;; 25
