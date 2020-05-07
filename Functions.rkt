#lang racket

;; inserts one atom to the left of another specified atom
(define insertL
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list))
               (cons new atomic-list))
              (else (cons (first atomic-list)
                          (insertL new old (rest atomic-list)))))))))

;; Replaces one atom with another atom
(define subst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (rest atomic-list)))
              (else (cons (first atomic-list) (subst new old (rest atomic-list)))))))))

;; inserts an atom to the right of each instance of a specified atom
(define multiinsertR
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list))
               (cons old
                     (cons new
                           (multiinsertR new old (rest atomic-list)))))
              (else (cons (first atomic-list) (multiinsertR new old (rest atomic-list)))))))))

;; same as multiinsertR but inserts to the left
(define multiinsertL
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list))
               (cons new
                     (cons old
                           (multiinsertL new old (rest atomic-list)))))
              (else (cons (first atomic-list) (multiinsertL new old (rest atomic-list)))))))))

;; repleaces each instance of an atom with another atom
(define multisubst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (multisubst new old (rest atomic-list))))
              (else (cons (first atomic-list) (multisubst new old (rest atomic-list)))))))))

;; recursive addition
(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (o+ a (sub1 b)))))))

;; recursive subtraction
(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))

;; adds elements of a tuple together
(define addtuple
  (lambda (tuple)
    (cond
      ((null? tuple) 0)
      (else (o+ (first tuple) (addtuple (rest tuple)))))))

;; recursive multiplication
(define o*
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (o+ a (o* a (sub1 b)))))))

;; add two tuples together
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (first tup1) (first tup2))
                  (tup+ (rest tup1) (rest tup2)))))))

;; recursive greater than
(define o>
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (o> (sub1 a) (sub1 b))))))

;; recursive less than
(define o<
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (o< (sub1 a) (sub1 b))))))
      
;; recrusive equal
(define o=
  (lambda (a b)
    (cond
      ((not (or (o> a b) (o< a b))) #t)
      (else #f))))

;; recursive exponent
(define o^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (o* a (o^ a (sub1 b)))))))

;; recursive division
(define o/
  (lambda (a b)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- a b) b))))))