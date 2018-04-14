#lang racket
;toBinary
(define (toBinary n)
  (if (= n 0) 0
      (+ (* 10 (toBinary (quotient n 2)))
         (remainder n 2))))