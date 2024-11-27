#lang racket
(define (my-map f l)
  (if (null? l) l (cons (f (car l)) (my-map f (cdr l)))))

(define (my-filter p l)
  ( cond ((null? l) l)
     ((p (car l)) (cons (car l) (my-filter p (cdr l))))
         (else (filter p (cdr l)))))

(define (accumulate op nv a b term next)
  (if (> a b) nv (op (term a) (accumulate op nv (next a) b term next))))

(define (foldr l op init)
  (if (null? l) init (op (car l) (foldr (cdr l) op init))))

(define (sum l) (foldr l + 0))

(define (forall? p l)
  (foldr (my-map p l) (λ (x y) (and x y)) #t))

(define (exists? p l)
  (foldr (my-map p l) (λ (x y) (or x y)) #f))

(define (my-member2 l x) 
  (exists? (λ (y) (equal? x y)) l))

(define (unique l)
  (foldr l (λ (x y) (if (and (not (null? y)) (equal? x (car y))) y (cons x y))) '() ))

