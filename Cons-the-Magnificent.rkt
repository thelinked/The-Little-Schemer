#lang racket

(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))


(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2 
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) 
       (cons new (cdr lat)))
      (else (cons (car lat) (subst new o1 o2 (cdr lat)))))))

(define multirember 
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) 
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? old (car lat)) 
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? old (car lat)) 
       (cons new (multiinsertL new old lat)))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
