#lang racket


(define make-account
  (λ(balance pwd)
    (define withdraw
      (λ(amount pwd^)
        (if (eqv? pwd pwd^)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                (error "Insufficient funds"))
            (error "Incorrect password"))))
    (define deposit
      (λ(amount pwd^)
        (if (eqv? pwd pwd^)
            (begin 
              (set! balance (+ balance amount)) balance)
            (error "Incorrect password"))))
    (define dispatch
      (λ(pwd op)
        (cond
          [(eq? op 'withdraw) (λ(v) (withdraw v pwd))]
          [(eq? op 'deposit) (λ(v) (deposit v pwd))])))
    dispatch))



(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;; 60

((acc 'secret-password 'deposit) 40)
;; 100

((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"

