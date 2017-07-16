
; closure is data structure
;(define (cons a b)
;  (lambda (i) ((cond ((= i 0) a)
;                     ((= i 1) b)))))

;(define (car pair)
;  (pair 0))

;(define (cdr pair)
;  (pair 1))


;(define x (cons 1 (cons 2 nil)))

;; 1
;(car x)

;; (cons 2 3)
;(cdr x)

;; 2
;(cdr (car x))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


