(define (average x y) (/ (+ x y) 2.0))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (newline)
  (display " init ")
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display " *** ")
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y)))
;               1.0))

;(sqrt 19)

(define (root-of-x-power-x x)
  (fixed-point (lambda (y) (/ (log x) (log y)) ) 2.0))

(define (root-of-x-power-x-damp x)
  (fixed-point (lambda (y) (average y (/ (log x) (log y)))) 2.0))

(root-of-x-power-x 1000)

(root-of-x-power-x-damp 1000)

