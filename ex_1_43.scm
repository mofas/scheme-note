(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn iter)
  (define (repeated-iter accFn i)
    (if (> i 1)
        (repeated-iter (compose fn accFn) (- i 1))
        accFn
        )
    )
  (repeated-iter fn iter)
  )


; 25
((repeated square 1) 5)

; 625
((repeated square 2) 5)

; 10
((repeated (lambda (x) (+ x 1)) 9) 1)
