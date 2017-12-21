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

(define (addtup tup)
  (cond ((null? tup) '())
        (else (+ (car tup) (addtup (cdr tup))))))

(define (tup+ tup1 tup2)
  (cond ((or (null? tup1) (null? tup2)) '())
        (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))


(define (occur* a lat)
  (cond ((null? lat) 0)
        ((atom? (car lat))
         (cond (eq? a (car lat))
               (add1 (occur* a (cdr lat)))
               (else (occur* a (cdr lat)))))
        (else (+ (occur* a (car lat)) (occur* a (cdr lat))))))


(define (subst* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond (eq? old (car l))
               (cons new (subst* new old (cdr l)))
               (else (cons old (subst* new old (cdr l)))))
         )
        (else (cons (subst* new old (car l) (subst* new old (cdr l)))))))


(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l))
         (or ((eq? a (car l)) #t) (member* a (cdr l))))
        (else (or (member* a (car l) (member* a (cdr l)))))))


(define (equal? a b)
  (cond ((and (atom? a) (atom? b)) (= a b))
        ((or (atom? a) (atom? b)) #f)
        (else (eqlist? a b))))

(define (eqlist? a b)
  (cond ((and (null? a) (null? a)) #t)
        ((or (null? a) (null? a)) #f)
        (else (and (equal? (car a) (car b)) (eqlist? (cdr a) (cdr b))))))

(define (numbered? exp)
  (cond ((atom? exp) (number? exp))
        (else (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp))))))))


(define (value exp)
  (cond ((atom? exp) exp)
        ((equal? (car (cdr a)) '+) (+ (value (car exp)) (value (car (cdr (cdr exp))))))
        ((equal? (car (cdr a)) '*) (* (value (car exp)) (value (car (cdr (cdr exp))))))
        ))





