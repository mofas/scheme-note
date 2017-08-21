(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (put '=zero? 'integer
       (lambda (x) (= x 0))))

(define (install-rational-number-package)
  (put '=zero? 'rational
       (lambda (x) (=zero? (numer x)))))

(define (install-complex-number-package)
  (put '=zero? 'complex
       (lambda (x) (and (=zero? (real-part x)) (=zero? (imag-part x))))))