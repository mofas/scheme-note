(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init sequence)
  (if (null? (car sequence))
      nil
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence)))))


; vector dot
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 3 2))

(define my-matrix-1 (list (list 1 2 3 4) (list 4 5 6 7) (list 6 7 8 9)))
(define my-matrix-2 (list (list 1 0 0) (list 0 1 0) (list 0 0 1) (list 0 0 0)))
(define my-vector-1 (list 0 1 0 1))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

; expect (6 11 16)
(matrix-*-vector my-matrix-1 my-vector-1)

(define
  (transpose mat)
  (accumulate-n cons nil mat))

; expect ((1 4 6) (2 5 7) (3 6 8) (4 7 9))
(transpose my-matrix-1)

(define (matrix-*-matrix m n)
 (let ((cols (transpose n)))
 (map (lambda (r) (matrix-*-vector cols r)) m)))

; expect ((7 10) (15 22))
(matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))

(matrix-*-matrix my-matrix-1 my-matrix-2)
