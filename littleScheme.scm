(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))


(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define (member? a lat)
  (cond ((null? lat) #f)
        (else (or
                (eq? a (car lat))
                (member? a (cdr lat))))))


(define (rember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat) (rember a (cdr lat))))))

(define (firsts lat)
  (cond ((null? lat) '())
        (else (cons (caar lat) (firsts (cdr lat))))))

(define (insertR new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new lod (cdr lat))))))

(define (insertL new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new lat))
        (else (cons (car lat) (insertR new lod (cdr lat))))))


(define (multirember a lat)
  (cond ((null? lat) #f)
        ((eq? a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat))))))
