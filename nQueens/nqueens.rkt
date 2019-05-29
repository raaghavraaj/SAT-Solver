#lang racket

(require "../Utils/2dvector.rkt"
         "../Utils/to_sat_utils.rkt"
         "../SAT/sat-solver.rkt"
         "../Utils/to_cnf.rkt"
         "../Utils/macros.rkt")

;; n is the number of queens
;; mode is either 'one or 'all, and corresponds to the number od solutions returned
(provide nqueens-solver)
(define (nqueens-solver n mode)
  (define numberzz (build-list n (lambda (x) (+ x 1))))

  (define row (node 'and (map (lambda (x) (make-node (map (lambda (y) (leaf (+ (* 10 x) y))) numberzz) 'unique)) numberzz)))
  (define column (node 'and (map (lambda (x) (make-node (map (lambda (y) (leaf (+ (* 10 y) x))) numberzz) 'unique)) numberzz)))


  (define (dia-help i j n)
    (let ([lst '()]
          [k i]
          )
      (begin (for (define l j) : (and (<= k n) (<= l n)) : (begin (set! l (+ l 1)) (set! k (+ k 1)))
               : (set! lst (cons (leaf (+ (* 10 k) l)) lst)))
             lst)))
  
  (define main-diagonal (node 'and (append (map (lambda (x) (make-node (dia-help x 1 n) 'atmost-one)) numberzz)
                                           (map (lambda (x) (make-node (dia-help 1 x n) 'atmost-one)) numberzz))))
  
  
  
  (define (a-dia-help i j n)
    (let ([lst '()]
          [k i])
      (begin (for (define l j) : (and (<= k n) (> l 0)) : (begin (set! l (- l 1)) (set! k (+ k 1)))
               : (set! lst (cons (leaf (+ (* 10 k) l)) lst)))
             lst)))
  
  (define aux-diagonal (node 'and (append (map (lambda (x) (make-node (a-dia-help x n n) 'atmost-one)) numberzz)
                                          (map (lambda (x) (make-node (a-dia-help 1 x n) 'atmost-one)) numberzz))))
  
  
  (define master (node 'and (list row column main-diagonal aux-diagonal)))

  (define (hash->2dvec hashish)
  (let* ([xx (hash->list hashish)]
        [xy (make-2d-vector n n #\_)]
        [xz (append (filter cdr xx) (list #f))])
    (begin (for (define i (car xz)): (not (boolean? i)) : (begin (set! i (cadr xz)) (set! xz (cdr xz))) :
             (2d-vector-set! xy (- (quotient (car i) 10) 1)  (- (remainder (car i) 10) 1) #\♣))
           xy)))

;  (define (hash->2dvec hashish)
;  (let* ([xx (hash->list hashish)]
;        [xy (make-2d-vector n n #\_)]
;        [xz (append (filter cdr xx) (list #f))])
;    (begin (for (define i (car xz)): (not (boolean? i)) : (begin (set! i (cadr xz)) (set! xz (cdr xz))) :
;             (2d-vector-set! xy (- (quotient (car i) 10) 1)  (- (remainder (car i) 10) 1) 'Q))
;           xy)))
  
  (cond [(equal? mode 'all)
                 (let ([solved (all-sat master 99 (make-hash))])
                   (if (equal? #f (car solved)) #f (map hash->2dvec solved)))]
        [(equal? mode 'one)
                 (let ([solved (sat-solver master 99 (make-hash))])
                   (if (equal? #f solved) #f (hash->2dvec solved)))]
        [else (error "Unknown Mode")]))
        