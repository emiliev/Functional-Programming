#lang racket
;revrse int
(define (log10 n) (/ (log n) (log 10)))

(define (count-digits n)
  (if (= n 0) 1
      (ceiling (log10 n)))
  )

;not working
(define (reverse-int!! n)
  (define rest
    (if (< n 10) 0 (reverse-int (quotient n 10))))
  (if (< n 10) n
      (+ (* (remainder n 10)
         (expt 10 (count-digits rest)))
      rest)))


(define (reverse-int n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10)
                (+ (* res 10) (remainder n 10)))))
  (if (or (negative? n)
      (not (integer? n)))
      #f
  (helper n 0)))