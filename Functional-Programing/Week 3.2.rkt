#|
Списъци:
 Свързани списъци, направени от двойки
   [ | ]->[ | ]->()

1. Основни структорни елементи
 - Null `() празен списък
 - Двойка: (cons <el1> <el2>)

 - (car <pair>) -> returns the first element
 - (cdr <pair>) -> returns the second element

|#

;Задача 1. (len l), която намира дължината на списък
(define (len l) (if (null? l) 0 (+ 1 (len(cdr l)))))
;2. (repeat n x), връща списък с n елемента, всеки от който е x
(define (repeat n x) (if (= n 0) '() (cons x (repeat (- n 1) x))))
; 3. (exists? l p), която проверява дали съществува елемент в l, за който е изпълнен предикатът p.
(define (exists? l p) (if (null? l) #f ( if (p (car l)) #t (exists? (cdr l) p))))
; 4. (member? l x), която проверява дали елементът x се съдържа в списъка l (какво значи "съдържа"?)
(define (member? l x) (if (null? l) #f (if (equal? x (car l)) #t (member? (cdr l) x))))
; 5. (at n l), която връща елементът, намиращ се на позиция n (броим от 0) в списъка l или #f, ако позицията е извън списъка
(define (at n l) (if (null? l) #f (if (= n 0) (car l) (at (- n 1) (cdr l)))))
; 6. (map f l), която прилага f върху всеки елемент на списъка l
(define (map1 f l) (if (null? l) '() (cons (f (car l)) (map1 f (cdr l)))))
(define (add1 x) (+ 1 x))
;7. (filter p l), която съставя нов списък, съдържащ само елементите на l, за които е изпълнен предикатът p
(define (filter p l) (if (null? l) '() (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))))
;8. (push x l), която добавя елемента x на края на списъка l
(define (push x l) (if (null? l) (cons x '()) (cons (car l) (push x (cdr l)))))
;9. (reverse l), която връща списък с елементите на l в обратен ред
(define (push_back x l)
  (if (null? l)
      (list x)
      (cons (car l) (push_back x (cdr l)))))

(define (reverse1 l) (if (null? l) '() (push (car l) (reverse1 (cdr l)))))
; 10. (insert x n l), която вкарва елемента x на позиция n в списъка l (ако n е след края на l, вкарваме x накрая)
(define (insert x n l) (if (null? l) (cons x '()) (if (= n 0) (cons x l) (cons (car l) (insert x (- n 1) (cdr l))))))

; 11. Дефинирайте функцията foldr, такава че (foldr l op init) пресмята (op l[0] (op l[1] (op l[2] ... (op l[n] init) ... )))
; (ако имаме подаден празен списък, резултатът е init). Тази функция е аналогична на функцията accumulate, която правихме миналия път.
(define (accumulate-n op init begin end)
  (if  (> begin end) init (op begin (accumulate-n op init (+ 1 begin) end))))

(define (foldr l op init) (if (null? l) init (op (car l) (foldr (cdr l) op init))))

; 12. (sum l), която намира сумата на числата в списък
(define (sum l) (foldr l + 0))

; 13. (forall? p l), която връща #t или #f в зависимост от това дали предикатът p е върнал #t за всеки елемент на l
(define (forall? p l)
  (foldr l (lambda (x acc) (and (p x) acc)) #t))

; 14. (exists? p l), която връща #t или #f в зависимост от това дали предикатът p е върнал #t за поне един елемент на l
(define (exists1? p l)
  (foldr l (lambda (x acc) (or (p x) acc)) #t))

; 15. (unique l), която превръща всеки няколко последователни срещания на една и съща стойност в списъка в единствено срещане на тази стойност
(define (unique l)
  (if (null? l)
      '()
      (foldr l
             (lambda (x acc)
               (if (and (not (null? acc)) (equal? x (car acc)))
                   acc
                   (cons x acc)))
             '())))


(define (is-bijection-over f items ))