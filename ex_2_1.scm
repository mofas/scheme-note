
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let (
        (start (start-segment segment))
        (end (end-segment segment))
        )
    (let (
          (mx (/ (+ (x-point start) (x-point end)) 2 ))
          (my (/ (+ (y-point start) (y-point end)) 2 ))
          )
      (make-point mx my))
    ))



(define (test x1 y1 x2 y2)
  (let (
        (start-point (make-point x1 y1))
        (end-point (make-point x2 y2))
        )
    (
     (let ((mp (midpoint-segment (make-segment start-point end-point))))
       (
          (print-point mp)
        )
       )
     )
    ))


(test 0 3 2 5)
(test -2 -1 2 5)