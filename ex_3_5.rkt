#lang racket

(require racket/trace)


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define estimate-integral
  (λ(pred x1 x2 y1 y2 trials)
    (define exper
      (λ() (pred (random-in-range x1 x2) (random-in-range y1 y2))))
    (monte-carlo trials exper)     
    ))


(define in-circle
  (λ(x y)
    (< 
     (+ (expt (- x 5) 2) (expt (- y 7) 2))
     9)
    ))


#;
(in-circle 5 7)



(estimate-integral in-circle 2 8 4 10 100000)
