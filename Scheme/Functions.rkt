#lang racket

;; atom is a string
;; atomic list is a list of atoms
;; tuple (tup) is a list of positive integers (natural numbers)

;; returns bool whether a value is atomic
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; returns bool whethere list is made of atomics
(define atomic-list?
  (lambda (i-list)
    (cond
      ((null? i-list) #t)
      ((atom? (first i-list)) (atomic-list? (rest i-list)))
      (else #f))))

;; returns bool whether an atom is present in an atomic list
(define member?
  (lambda (atom atomic-list)
    (cond
      ((null? atomic-list) #f)
      (else (or (eq? (first atomic-list) atom)
                (member? atom (rest atomic-list)))))))

;; returns atomic list with a specified atom removed
(define rember
  (lambda (atom atomic-list)
    (cond
      ((null? atomic-list)(quote ()))
      ((eq? (first atomic-list) atom)(rest atomic-list))
      (else (cons (first atomic-list)
              (rember atom (rest atomic-list)))))))

;; returns atomic list of the first atom in each atomic list
(define firsts
  (lambda (i-list)
    (cond
      ((null? i-list)(quote ()))
      (else (cons (first (first i-list)) (firsts (rest i-list)))))))

;; returns atomic list with new atom inserterd to the right of specified atom 
;; in the original atomic list
(define insertR
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? (first atomic-list) old)
               (cons old
                 (cons new (rest atomic-list))))
              (else (cons (first atomic-list)
                          (insertR new old (rest atomic-list)))))))))

;; returns atomic list with new atom inserted to the left of specified atom
;; in the original atomic list
(define insertL
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list))
               (cons new atomic-list))
              (else (cons (first atomic-list)
                          (insertL new old (rest atomic-list)))))))))

;; returns atomic list with one original atom replaced with a new atom
(define subst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (rest atomic-list)))
              (else (cons (first atomic-list) (subst new old (rest atomic-list)))))))))

;; returns atomic list with atoms inserted to the right of each instance of a specified atom
;; in original atomic list
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

;; returns atomic list with each instance of an atom original atom with a new atom
(define multisubst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (multisubst new old (rest atomic-list))))
              (else (cons (first atomic-list) (multisubst new old (rest atomic-list)))))))))

;; returns natural number throught recursive addition
(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (o+ a (sub1 b)))))))

;; returns natureal number throught recursive subtraction
(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))

;; returns natural number that is the sum of all numers in a tuple
(define addtuple
  (lambda (tuple)
    (cond
      ((null? tuple) 0)
      (else (o+ (first tuple) (addtuple (rest tuple)))))))

;; returns natural number through recursive multiplication
(define o*
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (o+ a (o* a (sub1 b)))))))

;; returns tuple where the numbers are the product of two tuples
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (first tup1) (first tup2))
                  (tup+ (rest tup1) (rest tup2)))))))

;; returns bool using recursive greater than
(define o>
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (o> (sub1 a) (sub1 b))))))

;; returns bool using recursive less than
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

;; returns bool using recursive exponent
(define o^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (o* a (o^ a (sub1 b)))))))

;; returns natural number using recursive division
(define o/
  (lambda (a b)
    (cond
      ((o< a b) 0)
      (else (add1 (o/ (o- a b) b))))))

;; returns natural number representing the number of atoms in an atomic list
(define length
  (lambda (atomic-list)
    (cond
      ((null? atomic-list) 0)
      (else (add1 (length (rest atomic-list)))))))

;; returns atom based on specified position
(define pick
  (lambda (index atomic-list)
    (cond
      ((zero? (sub1 index)) (first atomic-list))
      (else (pick (sub1 index) (rest atomic-list))))))

;; returns atomic list with atom removed from specified index
(define rempick
  (lambda (index atomic-list)
    (cond
      ((zero? (sub1 index)) (rest atomic-list))
      (else (cons (first atomic-list)
                  (rempick (sub1 index) (rest atomic-list)))))))

;; returns atomic-list from non-atomic list
(define no-nums
  (lambda (non-atomic-list)
    (cond
      ((null? non-atomic-list) (quote()))
      (else (cond
              ((number? (first non-atomic-list)) (no-nums (rest non-atomic-list)))
              (else (cons (first non-atomic-list)
                    (no-nums (rest non-atomic-list)))))))))

;; returns tuple from non-atomic list
(define all-nums
  (lambda (non-atomic-list)
    (cond
      ((null? non-atomic-list) (quote()))
      (else (cond
              ((number? (first non-atomic-list)) (cons (first non-atomic-list)
                                               (all-nums (rest non-atomic-list))))
              (else (all-nums (rest non-atomic-list))))))))

;; returns bool compares two numbers to see if they are the same
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

;; returns a count of the times an atom appears in an atomic list
(define occur
  (lambda (atom atomic-list)
    (cond
      ((null? atomic-list) 0)
      (else (cond
              ((eq? atom (first atomic-list)) (add1 (occur atom (rest atomic-list))))
              (else (occur atom (rest atomic-list))))))))

;; returns bool on whether an number equals one
(define one?
  (lambda (num)
    (o= 1 num)))

;; returns atomic-list where an atom has been removed from a specific index
;; function one? is used in this version
(define rempick2
  (lambda (index atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((one? index) (rest atomic-list))
              (else (cons (first atomic-list)
                          (rempick2 (sub1 index) (rest atomic-list)))))))))

;; returns atomic list where all instances of input atom are removed
(define rember*
  (lambda (atom atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      ((atom? (first atomic-list))
         (cond
           ((eq? atom (first atomic-list))
            (rember* atom (rest atomic-list)))
           (else (cons (first atomic-list)
                       (rember* atom (rest atomic-list))))))
      (else (cons (rember* atom (first atomic-list))
                  (rember* atom (rest atomic-list)))))))

;; returns atomic list where a new atom is inserted to the right of all instances of a specified atom
(define insertR*
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      ((atom? (first atomic-list))
         (cond
           ((eq? old (first atomic-list))
              (cons old
                    (cons new
                          (insertR* new old (rest atomic-list)))))
           (else (cons (first atomic-list)
                       (insertR* new old (rest atomic-list))))))
      (else (cons (insertR* new old (first atomic-list))
                  (insertR* new old (rest atomic-list)))))))
         
;; returns number of occurences of a specified atom
(define occur*
  (lambda (atom atomic-list)
    (cond
      ((null? atomic-list) 0)
      ((atom? (first atomic-list))
         (cond
           ((eq? atom (first atomic-list))
              (add1 (occur* atom (rest atomic-list))))
           (else (occur* atom (rest atomic-list)))))
      (else (o+ (occur* atom (first atomic-list))
                (occur* atom (rest atomic-list)))))))

;; returns list with specified atom replace with new atom
(define subst*
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      ((atom? (first atomic-list))
        (cond
          ((eq? old (first atomic-list))
            (cons new (subst* new old (rest atomic-list))))
          (else (cons (first atomic-list)
                      (subst* new old (rest atomic-list))))))
      (else (cons (subst* new old (first atomic-list))
                  (subst* new old (rest atomic-list)))))))

;; reutrns list with new atom to the left of specified atom
(define insertL*
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      ((atom? (first atomic-list))
       (cond
         ((eq? old (first atomic-list))
          (cons new
                (cons old
                      (insertL* new old (rest atomic-list)))))
         (else (cons (first atomic-list)
                     (insertL* new old (rest atomic-list))))))
       (else (cons (insertL* new old (first atomic-list))
                   (insertL* new old (rest atomic-list)))))))
