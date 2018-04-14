#lang racket
;natural digit?
(define (natural? n)
  (and (integer? n) (not (negative? n))))

;prime digit
(define (prime? n)
  (define (iterator i)
    (cond [(> i (sqrt n)) #t]
          [(= (remainder n i) 0) #f]
          [else (iterator (+ i 1))]))

  (if (= n 1)
      #f
      (iterator 2)))
