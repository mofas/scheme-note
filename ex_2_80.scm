(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0))))

(define (install-rational-number-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (=zero? (numer x)))))

(define (install-complex-number-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (and (=zero? (real-part x)) (=zero? (imag-part x))))))