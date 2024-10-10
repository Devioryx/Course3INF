(define (apply-with-2 f x) (f x 2))

(define (o f g)
  (define (comp-f-g x)
    (f (g x)))
  comp-f-g)

(define (double x) (* 2 x))

(define (add-1 x) (+ 1 x))

#|
Ламбда приема списък с аргументи, и след това тяло на дадената функция. Ламбда създава функции
Има два вида функции от по вискок ред

1. Функции които приемат функция като аргумент
2. Функции които връщат функции като резултат
|#

(define (o2 f g) (lambda (x) (f (g x))))
(define (repeated n f x) (if (= n 0) x (repeated (- n 1) f (f x))) )
(define (repeat n f) (lambda (x) (repeated n f x)))

(define (repeat-2 n f)
  ( if (= n 0) (lambda(x) x)
       (o f (repeat-2 (- n 1) f) )))

(define (repeat-3 n f)
  ( if (= n 0) (lambda(x) x)
       (lambda (x) (f (repeat-3 (- n 1) f) x))))

(define (repeat-4 n f)
  (repeated n (lambda (g) (o f g)) (lambda (x) x)))

(define (accumulate-n op init begin end)
  (if  (> begin end) init (op begin (accumulate-n op init (+ 1 begin) end))))

(define (accumulate-n1 op init f begin end)
  (if  (> begin end) init (op (f begin) (accumulate-n1 op init f (+ 1 begin) end))))

(define (fact x) (accumulate-n * 1 1 x))

(define (count p a b) (accumulate-n1 + 0 (lambda (x) (if (p x) 1 0)) a b))

(define (my-or x y) (if x #t y))

(define (exists? p a b)
  (accumulate-n1
   my-or
   #f
   p
   a
   b
   ))

(define (repeat-5 n f)
  (accumulate-n1
   o
   (lambda (x) x)
   (lambda (x) f)
   1
   n))

#|
`(+ 2 3)
Идеята е че всяко нещо се евалюеейтва само до себе си. Не се извършват никакви операции

3. Списъци - приема аргументи и ги вкарва в списък (list 1 2 3  + 42)

Как се представят вътрешно списъците
- рекурсивна структура
1. null ()
2. двойка (1 2 3) (1 *) (2 *) (3 *) () - като свързан списък
Има три функции за работа с тази двойка

1. (cons 1 2) -> (1 . 2)
2. (car (cons 1 2)) -> 1
3. (cdr (cons 1 2)) -> 2
4, (empty? (list)) #t
   (empty? (list 1 2 3)) #f

(car (list 1 2 3 4)) -> 1
(cdr (list 1 2 3 4)) -> (2 3 4)


|#

;(define (len1 x) (if (empty x) 0 (+ 1 (len1 (cdr x)))))