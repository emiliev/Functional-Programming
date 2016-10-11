#lang racket

;predefined if
(define (if2 t x y)
  (or (and t x) y))

;predefined and
(define (and2 x y)
  (if x y #f))

;predefined or
(define (or2 x y)
  (if x #t y))

;predefined not
(define (not2 x)
  (if x #f #t))

 ; (cond [(or (negative? n) (not integer? n))] #f)
  ;[(< n 2) n]
  ;[else (+ (+ (fib (- n 1)) (fib (- n 2))))])

;quadratic equation
(define (roots a b c)
  (define d (- (* b b) (* 4 a c)))
  (cond [(and (= a 0) (= b 0) 0)]
        [ (= a 0) 1]
        [ (> d 0) 2]
        [ (= d 0) 1]
        [else 0]))


(define (nchoosek n k)
(if (or (= k 0) (= k n))
  1
  (+ (nchoosek (- n 1) k)
     (nchoosek (- n 1) (- k 1))))
  )


(define (sq x) (* x x ))

;Fast power
(define (fast-exp x n )
  (define k (quotient n 2))
  (cond [(zero? n) 1]
        [(even? n) (sq (fast-exp x k))]
        [else (* (sq (fast-exp x k)) x)])
)

;fast exp 2
(define (fast-exp2 x n)
  (cond [(and (positive? n) (integer? n)) (fast-exp x n)]
        [(integer? n) (/ 1 (fast-exp x (- n)))]
        [else (* (fast-exp2 x (round n))
                 (expt x (- n (round n))))]
         )
  )


