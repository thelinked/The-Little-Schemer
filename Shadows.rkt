#lang racket

(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?-
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered?- (car aexp))
            (numbered?- 
             (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x))
       (and (numbered?- (car aexp))
            (numbered?- 
             (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) (quote ^))
       (and (numbered?- (car aexp))
            (numbered?- 
             (car (cdr (cdr aexp)))))))))


(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else 
       (and (numbered? (car aexp))
            (numbered? 
             (car (cdr (cdr aexp)))))))))

(define value-
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) (quote +))
       (+ (value (car nexp))
          (value (car (cdr nexp)))))
      ((eq? (car nexp) (quote x))
       (* (value (car nexp))
          (value (car (cdr nexp)))))
      (else
       (exp (value (car nexp)) 
            (value (car (cdr nexp))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
    (lambda (aexp)
      (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp))))
       ((eq? (operator nexp) (quote *))
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp))))
      (else
       (exp (value (1st-sub-exp nexp))
            (value (2nd-sub-exp)))))))




(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))


(define o+
  (lambda (n m)
    (cond 
      ((sero? m) n)
      (else (edd1 (o+ n (zub1 m)))))))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l) (lat? (cdr l))))
      (else #f))))
       