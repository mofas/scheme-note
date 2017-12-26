#lang racket

(require racket/trace)

 (define (count-pairs x) 
   (if (not (pair? x)) 
       0 
       (+ (count-pairs (car x)) 
          (count-pairs (cdr x)) 
          1))) 

(define count-pairs-fix
  (λ(ls)
    (let ((store '()))
      (define helper
        (λ(ls)
          (if (or (not (pair? ls))
                  (memq ls store))
              0
              (begin
                (set! store (cons ls store))
                (+ 1 (helper (car ls)) (helper (cdr ls))))
              )))
      (helper ls))))


(displayln "==== 3 -> 3 ====")
(define p3-3 '(a b c))
(count-pairs p3-3)
;; 3 -> 3
(count-pairs-fix p3-3)


(displayln "==== 3 -> 4 ====")
(define y '(b))
(define x (cons y y))
(define p3-4 (cons x '()))

(count-pairs p3-4)
;; 3 -> 4
(count-pairs-fix p3-4)


(displayln "==== 3 -> 7 ====")
(define p3-7 (cons x x))
;; 3 -> 7
(count-pairs p3-7)
(count-pairs-fix p3-7)

;; This is don't work in racket
;; 3 -> inf
;; (displayln "==== 3 -> inf ====")
;; (define u '(c))
;; (define v (cons u u))
;; (define p3-inf (cons v v))
;; (set-cdr! u p3-inf)
;; (count-pairs p3-inf)


