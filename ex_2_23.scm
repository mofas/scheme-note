

; the reason we need let declaration is otherwise it will try to excute
; previous return value

;(define (for-each proc items)
;  (let (tail (cdr items))
;    (proc (car items))
;    (if (not (null? tail))
;        ((for-each proc tail))
;        )
;    )
;  )


(define (for-each proc items)
  (proc (car items))
  (if (not (null? (cdr items)))
      ((for-each proc (cdr items)))
      )
  )



(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))