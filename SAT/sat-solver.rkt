#lang racket
(require "../Utils/to_cnf.rkt"
         "../Utils/to_sat_utils.rkt" ;; FOr testing
         "../Utils/sat-utils.rkt"
         "../Utils/macros.rkt"
         "SAT_testcases.rkt")

; main program to solve SAT
;; returns #f is not solvable
;; returns a single solution if solvable
(provide sat-solver)
(define (sat-solver tr n init-hash)
  (begin
    (define temp-ans (make-hash))
    (set! temp-ans (strict-merger temp-ans init-hash))
    (set! tr (eqn->cnf tr))
    (cond [(not (equal? #f temp-ans)) (set! temp-ans (strict-merger temp-ans (hash-after-unit-prop-binary-and-purity tr n)))])
    (define (helper tr ans-tbl)
      (begin
        (define curr-ans (if (not (equal? #f ans-tbl)) (eval-tr tr ans-tbl) #f))
        (cond [(equal? curr-ans #t) (completer ans-tbl n)]
              [(equal? curr-ans #f) #f]
              [(equal? #t tr) (completer ans-tbl n)]
              [(or (equal? #f ans-tbl) (equal? #f tr)) #f]
              [else (let ([new-var (return-first-variable tr ans-tbl)])
                      (cond [(equal? #f new-var) #f]
                            [(equal? 'none new-var) (cond [(eval-tr tr ans-tbl) (completer ans-tbl n)]
                                                          [else #f])]
                            [else (let* ([new-hash (binary-update
                                                    tr
                                                    (strict-merger ans-tbl
                                                                   (make-hash (list (cons new-var #t)))))]
                                         [new-tr (trim tr new-hash)]
                                         [ans-with-true (helper new-tr new-hash)])
                                    (cond [(not (equal? #f ans-with-true)) ans-with-true]
                                          [else (begin
                                                  (define newr-hash (binary-update
                                                                     tr
                                                                     (strict-merger ans-tbl
                                                                                    (make-hash (list (cons new-var #f))))))
                                                  (define new-tree (trim tr newr-hash))
                                                  (helper new-tree newr-hash))]))]))])))

    (cond [(equal? #f temp-ans) #f]
          [else
           (begin
             (set! temp-ans temp-ans)
             (define temp-trimmed-tree (trim tr temp-ans))
             (helper temp-trimmed-tree temp-ans))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Why stop at one? (Well, money, time, memory, life ...)
;; Returns all possible solutions of a sat equation
(provide all-sat)
(define (all-sat tr n init)
  (begin
    (define curr-ans (sat-solver tr n init))
    (define acc (list curr-ans))
    (while (not (equal? #f curr-ans))
           (begin             
             (set! tr (eqn->cnf (node 'and (list tr (node 'not
                                                          (list (node 'or
                                                                      (map (lambda (x) (node 'and
                                                                                             (map (lambda (y) (let ([index (car y)]
                                                                                                                    [bool (cdr y)])
                                                                                                                (if bool (leaf index)
                                                                                                                    (leaf (- index)))))
                                                                                                  x)))
                                                                           (map hash->list acc)))))))))
             (set! curr-ans (sat-solver tr n init))
             (cond [(not (equal? #f curr-ans)) (set! acc (cons curr-ans acc))])))
    acc))
  




