(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (1+ x) (+ 1 x))

(define (done? n)
  (= (+ n 2) (accumulate + 0 1 (- n 1) (lambda (x) (if (= 0 (remainder n x)) x 0)) 1+)))

(equal? (done? 20) #t)
(equal? (done? 28) #f)

(define (find-distance-up n)
  (if (done? n) 0
      (+ 1 (find-distance-up  (+ n 1)))))

(define (find-distance-down n)
  (if (done? n) 0
      (+ 1 (find-distance-down  (- n 1)))))

(define (find-min-distance n)
  (min (find-distance-up n) (find-distance-down n)))

(define (sum-almost-done a b)
  (accumulate + 0 a b (lambda (x) (if (and (< (find-min-distance x) (- x a)) (< (find-min-distance x) (- b x))) x 0)) 1+))

(equal? (sum-almost-done 5 24) 153)

(define (help-procedure f n l)
  (cond ((or (null? l) (= n 0) (symbol? (car l)) (symbol? (cadr l))) l)
        (else (help-procedure f (- n 1) (cons (f (car l) (cadr l)) (cddr l))))))

(define (run-machine l)
  (define (run-machine-helper arguments stack)
    (cond ((null? arguments) stack)
          ((or (number? (car arguments)) (symbol? (car arguments))) (run-machine-helper (cdr arguments) (cons (car arguments) stack)))
          ((procedure? (car arguments)) (run-machine-helper (cdr arguments) (map (lambda (x) (if (number? x) ((car arguments) x) x)) stack)))
          ((and (pair? (car arguments)) (procedure? (caar arguments)) (number? (cdar arguments))) (run-machine-helper (cdr arguments) (help-procedure (caar arguments) (cdar arguments) stack)))))
  (run-machine-helper l '()))

(equal? (run-machine (list 1 'x 4 'a 9 16 25 sqrt 6)) '(6 5 4 3 a 2 x 1))

(equal? (run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) '(45 a 2 x 1))
