(define (flip-horiz painter) (rotate90 (rotate90 (flip-vert painter))))
(define (rotate180 painter) (rotate90 (rotate90 painter)))
(define (rotate270 painter) (rotate90 (rotate90 (rotate90 painter))))