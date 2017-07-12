
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))
        )
    )
  (iter 0)
  )

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))
        )
    )
  (iter k 0)
  )


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)

