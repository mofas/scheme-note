(define (reverse items)
  (if (null? items)
      nil
      cons(reverse(cdr(items)), car(items))
      ))

(reverse (list 1 4 9 16 25))