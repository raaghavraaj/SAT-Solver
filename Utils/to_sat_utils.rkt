#lang racket
(require "../Utils/macros.rkt"
         "../Utils/2dvector.rkt")
(provide 2d->hash
         hash->2dvec
         hash-completer)

;; Utils for converting any problem to a tree

;; A HOF for building a node
;; Builds a node upto single depth only
;; mode can be 'and 'or 'not 'atmost-one 'unique
;; lst contains a list of Nodes / Leaf / Both that represent the variables
;; function will return a node after applying the given "mode" on all the variables
(provide make-node)
(define (make-node lst mode)
  ;; returns a node which returns true if one atmost one variable is true
  (define (atmost-one-node lst)
    (node 'and (lc (node 'not (list (node 'and (list x y)))) : x <- lst y <- lst @(> (index-of lst y) (index-of lst x)))))

  (cond [(equal? mode 'and) (node 'and lst)]
        [(equal? mode 'or) (node 'or lst)]
        [(equal? mode 'not) (node 'not lst)]
        [(equal? mode 'atmost-one) (atmost-one-node lst)]
        [(equal? mode 'unique) (let ([atmost-lst (node-list (atmost-one-node lst))]
                                     [atleast-element (node 'or lst)])
                                 (node 'and (cons atleast-element atmost-lst)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (2d->hash vec)
  (let ([tab (make-hash '())])
    (define (traverse i)
      (define (traversec j)
        (cond [(= j 9) '()]
              [(number? (2d-vector-ref vec i j)) (begin (hash-set! tab (+ (* 100 (+ 1 i)) (* 10 (+ 1 j)) (2d-vector-ref vec i j)) #t) (traversec (+ j 1)))]
              [else (traversec (+ j 1))]))
      (cond [(= i 9) '()]
            [else (begin (traversec 0) (traverse (+  i 1)))]))
    (begin (traverse 0) tab)))

(define (hash->2dvec hashish)
  (let* ([xx (hash->list hashish)]
         [xy (make-2d-vector 9 9 #f)]
         [xz (append (filter cdr xx) (list #f))])
    (begin (for (define i (car xz)): (not (boolean? i)) : (begin (set! i (cadr xz)) (set! xz (cdr xz))) :
             (2d-vector-set! xy (- (quotient (car i) 100) 1) (- (remainder (quotient (car i) 10) 10)  1) (remainder (car i) 10)))
           xy)))
    
;(define (hash-help vec lst)
;  (if (null? lst) vec
;      (let ([i (car lst)])
;        (begin
;          (2d-vector-set! vec (- (quotient (car i) 100) 1) (- (remainder (quotient (car i) 10) 10)  1) (remainder (car i) 10)))
;        (hash-help vec (cdr lst)))))

;; takes an input and completes the hash
;; returns a completed hash table
(define (hash-completer hash)
  ;; extends a given element
  ;; eg if elem is (cons 119 #t), then this returns (list (cons 111 #f) (cons 112 #f) ... (cons 119 #t))
  (define (extender elem)
    (let* ([number (car elem)]
           [prefix (quotient number 10)]
           [suffix (remainder number 10)]
           [truth (cdr elem)])
      (build-list 9 (lambda (x) (let ([curr-num (+ (* 10 prefix) (+ 1 x))])
                                  (cond [truth (if (= (+ 1 x) suffix) (cons curr-num #t) (cons curr-num #f))]
                                        [else (if (= (+ 1 x) suffix) elem (void))]))))))

  (make-hash (remove* (list (void)) (append* (map extender (hash->list hash))))))

