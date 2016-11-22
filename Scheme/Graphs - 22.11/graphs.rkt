#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)


;task1
(define test
  (make-tree 3
            (make-tree 1
                       empty-tree
                       (make-leaf 2))
            (make-tree 5
                       (make-leaf 9)
                       (make-leaf 3))))

(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

(define (tree-height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (tree-height (left-tree t))
                (tree-height (right-tree t))))))

;task2
;-inf.0 ; +inf.0

(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

;task3
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        (else (append (tree-level (- k 1) (left-tree t))
                       (tree-level (- k 1) (right-tree t))))))

;task4
(define (all-levels t)
  (let [(height (tree-height t))]
    (map (lambda (i) (tree-level i t)) (range 0 (+ height 1)))))

;task5
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))
;task6
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        ((< val (root-tree t)) (make-tree (root-tree t)
                                          (bst-insert val (left-tree))
                                          (right-tree t)))))
;task7
(define (list->tree list)
  (if (null? list)
      empty-tree
      (bst-insert (car list)
                  (list->tree  (cdr list)))))

;task8
(define (tree-sort lst)
  (tree->list (list->tree lst)))