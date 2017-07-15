(define (compose f g)
  (lambda (x) (f (g x)))

; 49
((compose square inc) 6)
