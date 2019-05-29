#lang racket
(require "../Utils/to_cnf.rkt"
         "../Utils/to_sat_utils.rkt" ;; FOr testing
         "../Utils/sat-utils.rkt"
         "../Utils/macros.rkt"
         "scheduler-utils.rkt")

; main program to solve SAT
;; returns #f is not solvable
;; returns a single solution if solvable
(provide sat-solver)
(define (sat-solver tr n init-hash days teams [TL-clauses '()])
  ;(displayln TL-clauses)
  (begin
    (define temp-ans (make-hash))
    (set! temp-ans (strict-merger temp-ans init-hash))
    (set! tr (eqn->cnf tr))
    (cond [(not (equal? #f temp-ans)) (set! temp-ans (strict-merger temp-ans (sch-hash-after-unit-prop-binary-and-purity tr n days teams)))])
    (define (helper tr ans-tbl)
      (begin
        (cond [(hash? ans-tbl) (displayln (length (hash->list ans-tbl)))])
        (cond [(node? tr) (displayln (length (node-list tr)))])
        (define TL-ans (eval-Tl TL-clauses ans-tbl))
        (define curr-ans (if (not (equal? #f ans-tbl)) (eval-tr tr ans-tbl) #f))
        (cond [(equal? #f tr) #f]
              [(equal? #f ans-tbl) #f]
              [(equal? #f TL-ans) #f]
              [(equal? curr-ans #t) (completer ans-tbl n)]
              [(equal? curr-ans #f) #f]
              [(equal? #t tr) (completer ans-tbl n)]
              [else (let ([new-var (return-first-variable tr ans-tbl)])
                      (cond [(equal? #f new-var) #f]
                            [(equal? 'none new-var) (cond [(eval-tr tr ans-tbl) (completer ans-tbl n)]
                                                          [else #f])]
                            [else (let* ([new-hash (sch-binary-update
                                                    tr
                                                    (strict-merger ans-tbl
                                                                   (scheduler-hash-completer
                                                                    (make-hash (list (cons new-var #t))) days teams)) days teams)]
                                         [new-tr (trim tr new-hash)]
                                         [ans-with-true (helper new-tr new-hash)])
                                    (cond [(not (equal? #f ans-with-true)) ans-with-true]
                                          [else (begin (displayln "Backtrack")
                                                  (define newr-hash (sch-binary-update
                                                                     tr
                                                                     (strict-merger ans-tbl
                                                                                    (scheduler-hash-completer
                                                                                     (make-hash (list (cons new-var #f))) days teams)) days teams))
                                                  (define new-tree (trim tr newr-hash))
                                                  (helper new-tree newr-hash))]))]))])))

    (cond [(equal? #f temp-ans) #f]
          [else
           (begin
             (set! temp-ans (scheduler-hash-completer temp-ans days teams))
             (define temp-trimmed-tree (trim tr temp-ans))
             (helper temp-trimmed-tree temp-ans))])))
