(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define one (lambda (f) (lambda (x) (f x) )))

(define two (lambda (f) (lambda (x) (f (f x)) )))
(define three (lambda (f) (lambda (x) (f (f (f x))) )))




; try to define addition procedure +
; one + two = three
; (add (f x) (f (f x)) ) = (f (f (f x)))
; the first we need to pass f to both a and b
; (a f) = (lambda (x) (f x) )
; (b f) (lambda (x) (f (f x)) )
; then we want to combine (a f) (b f) together
; to create (f (f (f x)))
; ((a f) ??(b f)??)
; I will use compose compose((a f) (b f))(x)
; Or ((a f) ((b f) x)
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x))
      ))

