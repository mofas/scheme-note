(define draw-line '())

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define v00 (make-vect 0 0))
(define v01 (make-vect 0 1))
(define v10 (make-vect 1 0))
(define v11 (make-vect 1 1))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))


(define outline
  (segments->painter
    (list
      (make-segment v00 v01)
      (make-segment v01 v11)
      (make-segment v11 v10)
      (make-segment v10 v00))))

(define x
  (segments->painter
    (list (make-segment v00 v11) (make-segment v01 v10))))


(define diamond
  (segments->painter
    (list
      (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
      (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))


