
(define (identity x) x)

(define (double x)
  (* x x))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; simpson integral
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (step x) (+ x (* 2 h)))
  (define (inner-term x)
    (+
      (f x)
      (* 4 (f (+ x h)))
      (f (+ x (* 2 h)))
      )
    )
  (/
    (* h (sum inner-term a step (- b (* 2 h))))
    3.0)
  )

(simpson cube 0 1 10)

(simpson double 0 1 10)

(simpson identity 0 1 10)