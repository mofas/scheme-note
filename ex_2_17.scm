(define (last-pair items)
  (define (last-pair-iter a prev)
    (if (null? a))
    prev
    (last-pair-iter cdr(a) car(a))
    )
  (last-pair-iter items, nil)
  )

(last-pair (list 23 72 149 34))

