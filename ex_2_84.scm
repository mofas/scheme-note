; 2.83

(define (raise x) (apply-generic 'raise x))

(put 'raise 'integer (lambda (x) (make-rational x 1)))
(put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x)))))
(put 'raise 'real (lambda (x) (make-from-real-img x 0)))


; 2.84

; stragety: raise x to see if x === y,
; if we cannot raise x anymore return false

(define (lower-than x y)
  (if (get 'raise (type-tag x))
      (let ((raised-x ((get 'raise (type-tag x)) x)))
        (if (= (type-tag raised-x) (type-tag y))
            #t
            (lower-than raised-x y)))
      #f
      ))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond ((lower-than a1 a2) (apply-generic op (raise a1) a2))
                      ((lower-than a2 a1) (apply-generic op a2 (raise a1)))
                      (else (error "No method for these types" (list op type-tags))))
                )
              (error "No method for these types" (list op type-tags)))))))


