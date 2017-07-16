(define (reverse items)
  (define (iter xs result)
    (if (null? xs)
        result
        (iter (cdr xs) (cons (car xs) result))
        )
    )
  (iter items '())
)

(reverse (list 1 4 9 16 25))