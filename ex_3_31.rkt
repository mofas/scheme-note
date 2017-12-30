#lang racket

(require racket/trace)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))


(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))


(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))


(define (print-aux ptr end)
  (begin
    (display (format "~a" (mcar ptr)))
    (if (not (eq? ptr end))
        (print-aux (mcdr ptr) end)
        (display "")
        ))
  )

(define (print-queue queue)
  (let ((start (front-ptr queue))
        (end (rear-ptr queue)))
    (if (null? start)
        (displayln "()")
        (begin
          (display "(")
          (print-aux start end)
          (displayln ")")))
    ))

(define q1 (make-queue))


(print-queue (insert-queue! q1 'a))

(print-queue (insert-queue! q1 'b))

(print-queue (delete-queue! q1))

(print-queue (delete-queue! q1))

(print-queue (insert-queue! q1 'c))

(print-queue (insert-queue! q1 'd))

(print-queue (insert-queue! q1 'e))

(print-queue (delete-queue! q1))
