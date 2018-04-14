#lang racket
(define (product a b term next)
  (if (> a b)
      1
      (* (term a) (product (next a) b term next))))

(define (sum a b term next)
 ;(if (> a b)
  ;    0
   ;   (+ (term a) (sum (next a) b term next))))
  (accumulate + 0 a b term next))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)

(define (++ x) (+ x 1))

(define (fact-accum n)
  (product 1 n id ++))


;x^n
(define (expt-accum x n)
  (define (getX k) x)
  (product 1 n getX ++))


;count divisors
(define (count-divisors n a b)
  (define (counter i)
    (if (= (remainder n i) 0)
        1
        0)
    )
  (sum a b counter ++))

;sum of powers
(define (powers-sum x n)
  (define (term i) (* i (expt x i)))
  (if (and (integer? n)
           (>= n 0))
      (sum 1 n term ++)
      #f))


(define (integrate f a b)
  (define h 0.001)
  (define (inc i) (+ i h))
  (define (ff a) (* (f a) h))
  (sum a b ff inc))
