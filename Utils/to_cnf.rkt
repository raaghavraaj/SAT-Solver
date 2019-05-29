#lang racket
(require "../Utils/macros.rkt")
(require "../SAT/SAT_testcases.rkt")
(provide eqn->cnf)
;; takes a boolean equation and converts it into the cnf format
(define (eqn->cnf tree)
  (match tree
    [(leaf val) (leaf val)]
    [(node 'or lst) (let ([eval-list (map (lambda (x) (let ([var (eqn->cnf x)])
                                                        (match var
                                                          [(leaf val) (cons var '())]
                                                          [(node 'not lst) (cons var '())]
                                                          [else (node-list var)]))) lst)])
                      ;;
                      (define (op x y)
                        (lc (cons a b) : a <- x  b <- y))
                      (define new-lst (foldl op '(()) eval-list))
                      (cond [(= 1 (length new-lst)) (node 'or (append* new-lst))]
                            [else (node 'and (map (lambda (x) (node 'or x)) new-lst))]))]
    [(node 'and list) (let ([eval-list (map (lambda (x) (let ([var (eqn->cnf x)])
                                                          (match var
                                                            [(leaf val) (cons var '())]
                                                            [(node 'not lst) (cons var '())]
                                                            [(node 'or lst) (cons var '())]
                                                            [else (node-list var)]))) list)])
                        (node 'and (append* eval-list)))]
    [(node 'not (cons a '())) (match a
                                [(leaf val) (leaf (- val))]
                                [(node op lst) (cond [(equal? op 'not) (eqn->cnf (car lst))]
                                                     [(equal? op 'and) (eqn->cnf (node 'or (map (lambda (x) (node 'not (cons x '()))) lst)))]
                                                     [(equal? op 'or) (eqn->cnf (node 'and (map (lambda (x) (node 'not (cons x '()))) lst)))]
                                                     [else (error "Unknown operation")])])]))