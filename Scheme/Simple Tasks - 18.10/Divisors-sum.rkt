#lang racket
(define (divisors-sum n)
  (define (helper i res)
    (cond [(< n i) res]
          [(= (remainder n 1) 0) (helper (+ i 1) (+ res 1))]
          [else (helper (+ i 1) res)]))
  (helper 1 0)
  )