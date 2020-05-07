#lang racket


(define insertL
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list))
               (cons new atomic-list))
              (else (cons (first atomic-list)
                          (insertL new old (rest atomic-list)))))))))
(define subst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (rest atomic-list)))
              (else (cons (first atomic-list) (subst new old (rest atomic-list)))))))))

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

(define multisubst
  (lambda (new old atomic-list)
    (cond
      ((null? atomic-list) (quote()))
      (else (cond
              ((eq? old (first atomic-list)) (cons new (multisubst new old (rest atomic-list))))
              (else (cons (first atomic-list) (multisubst new old (rest atomic-list)))))))))

(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (o+ a (sub1 b)))))))

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))

(define addtuple
  (lambda (tuple)
    (cond
      ((null? tuple) 0)
      (else (o+ (first tuple) (addtuple (rest tuple)))))))

(define o*
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (o+ a (o* a (sub1 b)))))))
