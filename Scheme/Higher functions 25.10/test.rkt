#lang racket
(define (sum-iter a b term next)
  (define (helper res curr)
    (if (> curr b)
        res
        (helper (+ res (term curr)) (next curr))))
  (helper 0 a))


;lambda
(define f (lambda (x y) (* x y)))
(define (f x y) (* x y))
((lambda (x y) (* x y)) 2 3)

(define (derivative f)
  (define h 0.00001)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

(define id-prim (derivative id))