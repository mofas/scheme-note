#lang racket


(define make-account
  (λ(balance pwd)
    (define withdraw
      (λ(amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Insufficient funds"))))
    (define deposit
      (λ(amount)
        (begin (set! balance (+ balance amount)) balance)))
    (define dispatch
      (λ(pwd^ op)
        (if (not (eqv? pwd pwd^))
            (error "Incorrect password")
            (cond
              [(eq? op 'withdraw) withdraw]
              [(eq? op 'deposit) deposit]
              ))))
    dispatch))

(define make-joint
  (λ(acc old-pwd new-pwd)
    (define dispatch
      (λ(pwd^ op)
        (if (not (eq? new-pwd pwd^))
            (error "Incorrect password")
            (acc old-pwd op))))
    dispatch))


(define acc (make-account 100 'secret-password))

#;
((acc 'secret-password 'withdraw) 40)
;; 60

#;
((acc 'secret-password 'deposit) 40)
;; 100

#;
((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"


(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))


((peter-acc 'open-sesame 'withdraw) 20)
;; 80

((paul-acc 'rosebud 'withdraw) 20)
;; 60


((peter-acc 'open-sesame 'deposit) 100)
;; 160

((paul-acc 'rosebud 'withdraw) 20)
;; 140
