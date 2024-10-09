(define Ex1 (+ (/ (+ 3 5) 2) (sqrt (- (* 4 4) (* 7 (* 2 2))))))
(define Ex2 (/ (+ 5 1/4 (- 2 (- 3 (+ 6 1/5)))) (* 3 (- 6 2) (- 2 7))))
(define Ex3 (/ (+ 15 21 (/ 3 15) (- 7 (* 2 2))) 16))

(define (my-not x) (if x #f #t))
(define (my-and x y) (if x (if y #t #f) #f))
(define (my-or x y) (if x #t (if y #t #f)))
(define (my-xor x y) (if x (if y #f #t) (if y #t #f)))

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(define (fib n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(define (count-digits n) (if (< n 10) 1 (+ 1 (count-digits (quotient n 10)))))

(define (succ n) (+ n 1))
(define (pred n) (define (predHelper n m) ( if (= n 0) 0 (if (= (succ m) n) m (predHelper n (succ m))))) (predHelper n 0))

(define (add a b) (define (addHelper a b m) (if(= b 0) a (addHelper (succ a) (pred b) m))) (addHelper a b 0))

(define (multiply a b) (define (multiplyHelper a b m) (if (= b 0) 0 (add a (multiplyHelper a (pred b) m)))) (multiplyHelper a b 0))

(define (my-fact n) (define (my-factHelper n m) (if (= n 0) 1 (multiply n (my-factHelper (pred n) m)))) (my-factHelper n 0))

;(define (divide a b)
; (define (divideHelper a b count)
;    (if (< a b)
;        count
;        (divideHelper (- a b) b (succ count))))
;  (if (= b 0)
;      (error "Division by zero")
;      (divideHelper a b 0)))

;(define (even1? n m) (if (= n 0) #t (if (= (pred n m) 0) #f (even1? (pred (pred n m) m) m))))

(define (safe-div n) (if (even? n) (quotient n 2) (quotient (- n 1) 2)))
(define (my-fib n) (if (= n 0) 0 (if (= n 1) 1 (add (fib (- n 1)) (fib (- n 2))))))