#lang racket


(require compatibility/mlist)

(require racket/trace)


(define (make-tree entry left right) 
  (mlist entry left right))

(define (entry tree)
  (mcar tree))

(define (left tree)
  (mcar (mcdr tree)))

(define (right tree)
  (mcar (mcdr (mcdr tree))))


(define (make-pair key value)
  (mcons key value))

(define (pair-key pair)
  (mcar pair))
(define (pair-value pair)
  (mcdr pair))


(define (adjoin-set x tree)
  (cond
    [(null? tree) (make-tree x '() '())]
    [(= (pair-key x) (pair-key (entry tree)))
     tree]
    [(< (pair-key x) (pair-key (entry tree)))
     (make-tree (entry tree) (adjoin-set x (left tree)) (right tree))
     ]
    [(> (pair-key x) (pair-key (entry tree)))
     (make-tree (entry tree) (left tree) (adjoin-set x (right tree)))
     ]
    ))

(define (make-table)
  (let ((local-table '()))
    (define (lookup key tree)
      (cond
        [(null? tree) #f]
        [(= key (pair-key (entry tree))) (entry tree)]
        [(< key (pair-key (entry tree))) (lookup key (left tree))]
        [(> key (pair-key (entry tree))) (lookup key (right tree))]
        ))
    (define (insert! key value)
      (let ((target (lookup key local-table)))
        (if target
            (set-mcdr! target value)
            (set! local-table (adjoin-set (make-pair key value) local-table))
            ))) 
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (λ(key)
                                    (let ((target (lookup key local-table)))
                                      (if target (pair-value target) #f)
                                      )))
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))



(define my-table (make-table))

((my-table 'lookup-proc) 1)
;; #f
((my-table 'lookup-proc) 2)
;; #f

((my-table 'insert-proc!) 1 'a)

((my-table 'lookup-proc) 1)
;; 'a
((my-table 'lookup-proc) 2)
;; #f


((my-table 'insert-proc!) 2 'b)
((my-table 'insert-proc!) 1 'c)


((my-table 'lookup-proc) 1)
;; 'c
((my-table 'lookup-proc) 2)
;; 'b


((my-table 'insert-proc!) 3 'e)
((my-table 'insert-proc!) 0 'f)


((my-table 'lookup-proc) 0)
;; 'f
((my-table 'lookup-proc) 1)
;; 'c
((my-table 'lookup-proc) 2)
;; 'b
((my-table 'lookup-proc) 3)
;; 'e
