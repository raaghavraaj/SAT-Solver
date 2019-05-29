#lang racket
(provide (all-defined-out))
(provide node
         node?
         node-list
         node-operation
         leaf
         leaf?
         leaf-val
         lc
         for
         while)
(struct node (operation list) #:transparent)
(struct leaf (val) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
     (begin
       init
       (define (iterate)
         (if condition (begin
                         statements
                         step
                         (iterate))
             (void)))
       (iterate))]))

(define-syntax while
  (syntax-rules ()
    [(while bool statements) (begin
                               (define (iterate)
                                 (cond [bool (begin
                                               statements
                                               (iterate))]))
                               (iterate))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
