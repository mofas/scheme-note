
(define (equal? as bs)
  (cond ((and (not (pair? as)) (not (pair? bs))) (eq? as bs))
        ((and (pair? as) (pair? bs))
         (and (equal? (car as) (car bs)) (equal? (cdr as) (cdr bs)) ))
        (else false))
  )



(equal? 1 1)
(equal? 1 2)

(equal? 'a 'a)
(equal? 'a 'b)

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))