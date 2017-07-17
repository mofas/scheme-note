(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (op-vector op v1 v2)
  (cons (op (xcor-vect v1) (xcor-vect v2)) (op (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2) (op-vector + v1 v2))
(define (sub-vect v1 v2) (op-vector - v1 v2))
(define (scale-vect s v) (cons (* s (xcor-vect v1)) (* s (ycor-vect v))))

; testing
(define v1 (make-vect 1 2))
(define v2 (make-vect 3 1))

; expect (4 3)
(add-vect v1 v2)
; expect (-2 1)
(sub-vect v1 v2)
; expect (2 4)
(scale-vect 2 v1)