#lang racket

(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sub1
  (lambda (n)
    (- n 1)))

(define zero?
  (lambda (n)
    (eq? n 0)))

(define one?
  (lambda (n)
    (eq? n 1)))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons (s1 (cons s2 (quote ()))))))

(define a-pair?
  (lambda (x)
    (cond 
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define revpair
  (lambda (pair)
    (build (second pair) (second pair))))

(define even?
  (lambda (n)
    (= (* (/ n 2) 2) n)))


(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn) lat))
      (else (eq? sorn a)))))
      
      
(define pick
  (lambda (n lat)
    (cond
      ((zero? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else 
       (+ (length* (first pora))
          (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond 
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (add1 (* 3 n)))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n)(add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(define length
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(define almost-length
  (lambda (f)
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 (f (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))   
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(define lengthY (Y almost-length))
