
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


; list
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

; list map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; list filter
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; list reduce
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; list generator
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; list flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; list remove
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))


; tree
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)



; nested loop
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))



