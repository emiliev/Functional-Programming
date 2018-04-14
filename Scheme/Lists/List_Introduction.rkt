#lang racket

;map
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))


;list lenght
(define (length* lst)
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))))

;iterative
(define (lengthI lst)
  (define (helper lst res)
    (if (null? lst) res
        (helper (cdr lst) (+ res 1))))
  (helper lst 0))

;reverse
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))


;reverse-iterative
(define (reverseI lst)
  (define (helper list res)
    (if (null? lst)
        res
        (helper (cdr lst) (cons (car lst) res))))
  (helper lst '()))


;n-th
(define (nth n lst)
  (cond [(null? lst) #f]
        [(= n 0) (car lst)]
        [else (nth (- n 1) (cdr lst))]))


(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ from 1) to))))

(define (take* n lst)
  (cond [(= n 0) '()]
        [(null? lst) '()]
        [else (cons (car lst)
                    (take* (- n 1) (cdr lst)))])
  )


(define (drop* n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop* (- n 1) (cdr lst))])
)

(define (chunk n lst)
  (if (null? lst) '()
      (cons (take* n lst)
            (chunk n (drop* n lst)))))

(define (all p? lst)
  (cond [(null? lst) #t]
        [(not (p? (car lst))) #f]
        [else (all p? (cdr lst))]))


(define (any* p? lst)
  (not (all (lambda (x) (not (p? x))) lst)))


(define (foldr* op nv lst)
  (if (null? lst)
      nv
      (op (car lst)
          (foldr* op nv (cdr lst)))))

























            