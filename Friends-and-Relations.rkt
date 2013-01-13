#lang racket

(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan? 
  (lambda (a1 a2)
    (cond
      ((and (number? a1 ) (number? a2))
       (= a1 a2))
      ((or (number? a1 ) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1 s2)) 
       #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (car l1) (cdr l2)))))))

(define member? 
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

(define multirember 
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((equal? (car lat) a) 
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))



(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset1
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat))
       (makeset1 (car lat)))
      (else (cons (car lat) (makeset1 (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (car lat)
                  (makeset
                   ((multirember (car lat)
                                 (cdr lat)))))))))
(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (car (car l)) (firsts (cdr l)))))))



(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define interset
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or ((member (car set1) set2) #t)
                (interset (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set) (car l-set)))
      (else (interset (car l-set))
            (intersectall (cdr l-set))))))

(define a-pair
  (lambda (x)
    (cond 
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons (s1 (cons s2 (quote ()))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (second pair))))

(define revel
  (lambda (rel)
    (cond 
      ((null? rel) (quote ()))
      (else (cons (build (revpair car rel)))))))

(define fullfun?
  (lambda (fun)
    (fun? (revel fun))))