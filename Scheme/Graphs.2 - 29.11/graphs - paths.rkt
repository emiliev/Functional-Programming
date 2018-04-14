#lang racket

(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))

(define (member? x lst)
  (if (member x lst) #t #f))

;task1
(define (vertices g)
  (map car g))

(define (successors v g)
  (let [(result (assoc v g))]
    (if result (cdr result) result)))

(define (has-edge? u v g)
  (if (member v (successors u g)) #t #f))

(define (add-vertex v g)
  (if (member v (vertices g))
      g
      (cons (list v) g)))


;task2
(define (add-edge u v g)
  (let [(newg (add-vertex u (add-vertex v g)))]
    (map (lambda (l) (if (equal? (car l) u)
                         (append l (list v))
                         l)) newg)))

(define (make-from-edges lst)
;  (if (null? lst)
 ;     '()
  ;    (add-edge (caar lst) (cdar lst)
   ;             (make-from-edges (cdr lst)))))
  (foldr (lambda (e g) (add-edge (car e)
                                 (cdr e)
                                 g))
         '()
         lst))

;task3
(define (contains-path? path g)
  (cond [(null? path) #t]
        [(null? (cdr path)) (member? (car path) (vertices g))]
        [(has-edge? (car path) (car path) g) (contains-path? (cdr path) g)]
        [else #f]))

(define (contains-path?? path g)
  (define (make-pairs path) #f)
  (filter (lambda (e) (not (has-edge? (car e)
                                      (cdr e)
                                      g))))
  (make-pairs path))

(define (map* f lst)
  (foldr (lambda (x l) (cons (f x) l)) '() lst))

(define (filter* p? lst)
(foldr (lambda (x l) (if (p? x)
                         (cons x l)
                         l))
       '()
       lst))


;taks4
(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g)) (vertices g)))

;task5
(define (extend-path path g)
  (if (null? path)
      (map list (vertices g))
      
      (map (lambda (v) (append path (list v)))
           (filter (lambda (v) (not (member? v path)))
                   (successors (last path) g)))))

;task6
(define (extend-paths paths g)
  (apply append ((map (lambda (p) (extend-path p g)) paths))))

;task7
(define (edge-list g)
  (define (make-pairs-single l) (map (lambda (v)  (cons (car l) v)) (cdr l)))
  (apply append (map make-pairs-single g)))

;task8
(define (invert g)
  (define (flip p) (cons (cdr p) (car p)))
  (make-from-edges (map flip (edge-list g))))

;task9
(define (bfs v g)
  (define (get-next-level current visited)
    (apply append (map (lambda (v) (filter (lambda (v) (not (member? v visited)))
                             (successors v g))) current)))
        (define (helper current result)
          (let [(next (get-next-level current result))]
            (if (null? next)
                result
                (helper next (append result next)))))
  (helper (list v) (list v)))

;task10
(define (find-path u v g)
  