#lang racket
(define (divisors-sum n)
  (define (helper i res)
    (cond [(< n i) res]
          [(= (remainder n i) 0) (helper (+ i 1) (+ res i))]
          [else (helper (+ i 1) res)]))
  (helper 1 0)
  )

;perfect digit
(define (perfect? n) (= (divisors-sum n) (* 2 n)))

      
      
        