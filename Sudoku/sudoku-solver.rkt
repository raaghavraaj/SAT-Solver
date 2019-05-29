#lang racket

(require "../Utils/to_cnf.rkt")
(require "sat-solver-sudoku.rkt")
(require "../Utils/2dvector.rkt"
         "../Utils/macros.rkt"
         "../Utils/to_sat_utils.rkt"
         "../Utils/sat-utils.rkt"
         "Sudoku_testcases.rkt")
(provide sudoku-solver)

;; each cell represented as xyz (729 variables)
;; xyz = true means element is xth row, yth column has z in it is true

(define (remone lis ris ans)
  (if (null? ris) ans
      (remone (append lis (list (car ris))) (cdr ris) (append ans (list (append lis (cdr ris)))))))

(define (oneinlist lis)
  (make-node lis 'unique))

(define numberzz '(1 2 3 4 5 6 7 8 9))



(define cell  (node 'and (map (lambda (x) (oneinlist x)) (map (lambda (a) (map (lambda (b) (leaf (+ a b))) numberzz))
                                                              (lc (+ (* 100 x) (* 10 y)) : x <- numberzz y <- numberzz)))))


(define row (node 'and (map (lambda (x) (oneinlist x)) (map (lambda (a) (map (lambda (b) (leaf (+ a (* 10 b)))) numberzz))
                                                            (lc (+ (* 100 x) (* 1 y)) : x <- numberzz y <- numberzz)))))

(define column (node 'and (map (lambda (x) (oneinlist x)) (map (lambda (a) (map (lambda (b) (leaf (+ a (* 100 b)))) numberzz))
                                                               (lc (+ (* 1 x) (* 10 y)) : x <- numberzz y <- numberzz)))))

(define (num x y z)
  (+ (* 100 x) (* 10 y) z))

(define (help-threex3 x y z) (oneinlist (map leaf (list (num x y z) (num x (+ y 1) z) (num x (+ y 2) z)
                                                        (num (+ 1 x) y z) (num (+ 1 x) (+ y 1) z) (num (+ 1 x) (+ y 2) z)
                                                        (num (+ 2 x) y z) (num (+ 2 x) (+ y 1) z) (num (+ 2 x) (+ y 2) z)))))

(define skip1 '(1 4 7))

(define threex3 (node 'and (map (lambda (x) (node 'and x)) (lc (map (lambda (z) (help-threex3 x y z)) numberzz) : x <- skip1 y <- skip1))))

(define master (node 'and (list row column threex3 cell)))



(define (sudoku-solver vec)
  (let ([hashed (sat-solver master 1000 (hash-completer (2d->hash vec)))])
    (cond [(equal? #f hashed) #f]
          [else (hash->2dvec (make-hash (filter (lambda (x) (equal? #t (cdr x))) (hash->list hashed))))])))
