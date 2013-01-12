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

(define rember*
  (lambda (a l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l)
       (cond 
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l)))))))
       (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*  ;Inserts "new" to the right of "old" in the nested list "l"
  (lambda (new old l)
    (cond 
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old 
                (cons new 
                      (insertR* new old (cdr l)))))
         (else (cons (car l) 
                     (insertR* new old (cdr l))))))
       (else (cons insertR* (car l))
             (insertR* new old (cdr l))))))

(define occur* ;Counts the amount of times an atom is in a nested list
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? car l)
       (cond
         ((eq? (car l) a)
          (add1 (occur* (cdr l))))
         (else (occur* (cdr l)))))
      (else (+ (occur* (car l))
               (occur* (cdr l)))))))

(define subst* ;Replace the atom old with new in a nested  list
  (lambda (new old l)
    {cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l (subst* (new old (cdr l))))))))
      (else 
       (cons (subst* new old (car l))
             (subst* new old (cdr l))))}))
          
(define insertL* ;Inserts "new" to the left of "old" in the nested list "l"
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) 
          (cons new (cons old 
                          (insertL* new old (cdr l)))))
         (else (cons (cons (car l) 
                           (insertL* new old (cdr l)))))))
       (else (cons (insertL* new old (car l))
                   (insertL* new old (cdr l)))))))


(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (leftmost (cdr l)))))


(define eqlist?-old
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist?-old (car l1) (car l2))))
      ((or (atom? (car l1))
           (atom? (car l2)))
       #f)
      (else 
       (and (eqlist?-old (car l1) (car l2))
            (eqlist?-old (car l1) (car l2)))))))

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


(define rember
  (lambda (s l)
    (cond
      ((null? l)(quote()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))

