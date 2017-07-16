(define (even-parity? x)
  (= (remainder x 2) 0))

;(even-parity? 3)
;(even-parity? 4)

(define (filter pred xs)
  (if (null? xs)
      xs
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs))
          )
      )

  )

;(filter (lambda (x) (< x 3)) (list 1 2 3 4 5))
;(filter (lambda (x) (> x 3)) (list 1 2 3 4 5))
;(filter even-parity? (list 1 2 3 4 5 6 7))

(define (same-parity x . xs)
  (if (even-parity? x)
    (filter (lambda (x) (even-parity? x)) (cons x xs))
    (filter (lambda (x) (not (even-parity? x))) (cons x xs))
  ))


(same-parity 1 2 3 4 5 6 7)
;(1 3 5 7)

(same-parity 2 3 4 5 6 7)
;(2 4 6)