(define (inc n) (+ n 1))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 5)

; integral
(define (integral f a b dx)
 (define (add-dx x) (+ x dx))
 (* (sum f (+ a (/ dx 2.0)) add-dx b)
 dx))

