#lang racket

(define (mat-at m i j)
  (list-ref (list-ref m i) j))

(define (mat-map f m)
  (map (λ (row) (map f row)) m))

(define (all-numbers? l)
  (if (null? l) #t (and (number? (car l)) (all-numbers? (cdr l)))))

(define (rows-valid? rows row-length)
  (if (null? rows) #t (let ((row (car rows))) (and (list? row) (=(length row) row-length) (all-numbers? row) (rows-valid? (cdr rows) row-length)))))

(define (mat? m)
  (and (list? m) (not (null? m)) (let ((row-length (length (car m)))) (rows-valid? m row-length))))

(define (scalmul x m)
  (mat-map (λ (y) (* x y)) m))

(define (transpose m)
  (if (null? (car m)) '() (cons (map car m) (transpose (map cdr m)))))

(define (product v1 v2)
  (apply + (map * v1 v2)))

(define (matmul m n)
  (let ((n-transposed (transpose n)))
    (map (λ (m-row)
           (map (lambda (n-col)
                  (product m-row n-col))
                n-transposed))
         m)))


(define (safe-mat-at m i j)
  (if (and (>= i 0) (< i (length m))
           (>= j 0) (< j (length (list-ref m 0))))
      (mat-at m i j)
      0))

(define (get-3x3-submatrix m i j)
  (map (lambda (row)
         (map (lambda (col)
                (safe-mat-at m row col))
              (list (- j 1) j (+ j 1))))
       (list (- i 1) i (+ i 1))))


(define (apply-kernel kernel mat)
  (let ((nrows (length mat))
        (ncols (length (car mat))))
    (map (lambda (i)
           (map (lambda (j)
                  (kernel (get-3x3-submatrix mat i j)))
                (range 0 ncols)))
         (range 0 nrows))))
