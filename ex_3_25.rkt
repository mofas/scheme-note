#lang racket


(require compatibility/mlist)

(require racket/trace)



(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup keys table)
      (cond
        [(null? keys) (mcdr table)]
        [else
         (let ((target (massoc (car keys) (mcdr table))))
           (if target
               (lookup (cdr keys) target)
               #f))]
       ))
    (trace-define (insert! keys value table)
      (let ((target (massoc (car keys) (mcdr table))))
        (if (null? (cdr keys))
            ;; insert or update value
            (if target
                (set-mcdr! target value)
                (set-mcdr! table (mcons (mcons (car keys) value) (mcdr table))))
            (if target
                ;; nested search 
                (insert! (cdr keys) value target)
                
                ;; create new table
                (let ((new-table (mlist (car keys))))
                  (begin
                    (set-mcdr! table
                               (mcons new-table (mcdr table)))
                    (insert! (cdr keys) value new-table))) 
                )))) 
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (λ(keys) (lookup keys local-table)))
            ((eq? m 'insert-proc!) (λ(keys value) (insert! keys value local-table)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))



(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(put '(a) 1)
;; (get '(a))
;; ;; 1

;; (get '(b))
;; ;; false

;; (get '(a b))
;; ;; false

;; (get '(b c d))
;; ;; false


(put '(b) 2)
(get '(b))
;; 2

;; we cannot create table (b) 2, (b c) 3
;; in the same time

;; (put '(b c) 3)
;; (get '(b c))
;; 3

;; (put '(d e) 4)

;; (get '(d e))
;; ;; 4
