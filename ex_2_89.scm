
; term is pair (2 1), and term-list is list -> (3 0 0 2 0 1)
; (adjoin-term (2 1) (3 0 0 2 0 1))
; will be (3 0 0 3 0 1)

; term is pair (4 1), and term-list is list -> (1 0)
; (adjoin-term (4 1) (1 0))
; will be (1 0 0 0 1 0)

(define (adjoin-term term term-list)
  (let (
        (coeff-of-term (coeff term))
        (order-of-term (order term)))
    (
     (cond (=zero? (coeff term)) term-list
           ; if the highest order of term list is small than the order of term
           ; we padding 0 to term-list to highest-order term
           (> order-of-term (length term-list)) (adjoin-term term (cons 0 term-list))
           ; if the highest order of term-list is larger than the order of term
           ; than we peel our term-list highest coeff and make it recursive
           ; then it will fallback to the third scienaro
           (< order-of-term (length term-list))
           (cons (car term-list) (adjoin-term term (cdr term-list)))
           ; if term and term-list has the same order,
           ; then we add the coeff of term and the highest term coeff together
           (else (cons (add coeff-of-term (car term-list)) (cdr term-list))))
     )))


(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))