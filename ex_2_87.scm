(define (=zero? x) (apply-generic '=zero? x))

(define (install-polynomial-number-package)
  (put '=zero? 'polynomial
       (lambda (x)
         ; if this is pair, then it is non empty term list
         (cond ((pair? x) #f)
               ; then we need to check it is 0 (integer, rational, real)
               ; I don't assume we have "drop" implementation here
               (else (=zero? x))))))
