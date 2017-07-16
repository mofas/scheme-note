(define (fringe tree)
  (define (iter xs result)
    (cond
      ((null? xs) result)
      ((not (pair? xs)) (cons xs result))
      (else (iter (car xs) (iter (cdr xs) result)))))
  (iter tree '())
  )



(fringe (list 1 2))

(define x (list (list 1 2) (list 3 4)))

;(1 2 3 4)
(fringe x)

;(1 2 3 4 1 2 3 4)
(fringe (list x x))

