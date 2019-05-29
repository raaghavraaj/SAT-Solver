#lang racket
(require "../Utils/macros.rkt"
         "../Utils/sat-utils.rkt")
(provide to-term
         term-day
         term-t1
         term-t2)

 
(define (to-term day t1 t2) (+ t2 (* 100 t1) (* 10000 day)))
(define (term-day term) (quotient term 10000))
(define (term-t1 term) (remainder (quotient term 100) 100))
(define (term-t2 term) (remainder term 100))

;; takes an input and completes the hash
;; returns a completed hash table
(provide scheduler-hash-completer)
(define (scheduler-hash-completer hash days teams)
  ;; ensures that all other days have #f for the same match
  (define (match-unique elem)
    (let* ([number (car elem)]
           [truth (cdr elem)]
           [day (term-day number)]
           [t1 (term-t1 number)]
           [t2 (term-t2 number)])
      (if (not truth) (list elem)
          (cons (cons number #t) (lc (cons (to-term d t1 t2) #f) : d <- days @(and (not (= d day))))))))
                                                                                                               
  (define (same-day elem)
    (let* ([number (car elem)]
           [truth (cdr elem)]
           [day (term-day number)]
           [t1 (term-t1 number)]
           [t2 (term-t2 number)])
      (if (not truth) (list elem)
          (lc (cons (to-term day it1 it2) #f) : it1 <- teams it2 <- teams @(and (not (= it1 it2))
                                                                                (not (and (= it1 t1) (= it2 t2)))
                                                                                (or (= it1 t1)
                                                                                    (= it1 t2)
                                                                                    (= it2 t1)
                                                                                    (= it2 t2)))))))
      
    
  (define (adjacent-days elem)
    (let* ([number (car elem)]
           [truth (cdr elem)]
           [day (term-day number)]
           [t1 (term-t1 number)]
           [t2 (term-t2 number)])
      (if (not truth) (list elem)
          (lc (cons (to-term d it1 it2) #f) : d <- days it1 <- teams it2 <- teams @(and (not (= it1 it2))
                                                                                        (not (and (= it1 t1) (= it2 t2)))
                                                                                        (or (= it1 t1)
                                                                                            (= it1 t2)
                                                                                            (= it2 t1)
                                                                                            (= it2 t2))
                                                                                        (or (= d (+ day 1))
                                                                                            (= d (- day 1))))))))
          
  (make-hash (append
              (append* (map match-unique (hash->list hash)))
              (append* (map same-day (hash->list hash)))
              (append* (map adjacent-days (hash->list hash))))))

;; evaluates Cl, which has a list of Tl clauses
(provide eval-Tl)
(define (eval-Tl Cl ans-tbl)
  (cond [(equal? #f ans-tbl) #f]
        [else
         (define (helper Cl)
           (define (val->bool val)
             (if (hash-has-key? ans-tbl (abs val)) (if (positive? val) (hash-ref ans-tbl val) (not (hash-ref ans-tbl (- val))))
                 #f))
           (let* ([lmt (car Cl)]
                  [lst (cdr Cl)]
                  [temp (length (remove* '(#f) (map (lambda (x) (if (hash-has-key? ans-tbl (abs x)) (val->bool x) #f)) lst)))])
             (or (= temp 0) (and (> lmt temp) (<= 1 temp)))))

         (let ([res (map helper Cl)])
           (if (list? (member #f res)) #f #t))]))

(provide sch-binary-update)
(define (sch-binary-update tr ans-tbl days teams)
  (define ans ans-tbl)
  ;; takes an 'or node and returns a list of possible answers
  ;; returns (void) if the length is not 2
  ;; returns #t if both are present and the clause is obsolete
  ;; returns #f if both are false
  (define (or-process nd)
    (let ([lst (node-list nd)])
      (cond [(not (= (length lst) 2)) (void)]
            [else (let ([cl1 (leaf-val (car lst))]
                        [cl2 (leaf-val (cadr lst))])
                    (cond [(hash-has-key? ans-tbl (abs cl1)) (cond [(hash-has-key? ans-tbl (abs cl2)) (let*
                                                                                                          ([val1 (hash-ref ans-tbl (abs cl1))]
                                                                                                           [val2 (hash-ref ans-tbl (abs cl2))]
                                                                                                           [cl1val (if (positive? cl1) val1 (not val1))]
                                                                                                           [cl2val (if (positive? cl2) val2 (not val2))])
                                                                                                        (or cl1val cl2val))]
                                                                   [else (let* ([val1 (hash-ref ans-tbl (abs cl1))]
                                                                                [cl1val (if (positive? cl1) val1 (not val1))])
                                                                           (if cl1val (void)
                                                                               (if (positive? cl2) (make-hash (cons (cons (abs cl2) #t) (hash->list ans)))
                                                                                   (make-hash (cons (cons (abs cl2) #f) (hash->list ans))))))])]
                          [(hash-has-key? ans-tbl (abs cl2)) (or-process (node 'or (list (leaf cl2) (leaf cl1))))]
                          [else (void)]))])))

  ;; main body
  (cond [(equal? #f ans-tbl) #f]
        [else
         (match tr
           [#t ans-tbl]
           [#f #f]
           [(leaf val) (make-hash)]
           [(node 'or lst) (let ([res (or-process tr)])
                             (if (equal? res #t) ans-tbl res))]
           [(node 'and lst) (let ([tlst (remove* (list #t (void)) (map (lambda (x) (if (node? x) (or-process x) (void))) lst))])
                              (cond [(list? (member #f tlst)) #f]
                                    [else (let ([res (foldl strict-merger (make-hash) tlst)])
                                            (if (equal? #f res) #f (strict-merger (scheduler-hash-completer res days teams) ans-tbl)))]))])]))


;; returns a final hash-table after unit-propogation and purity check
;; assumes the tree to be in cnf format
(provide sch-hash-after-unit-prop-binary-and-purity)
(define (sch-hash-after-unit-prop-binary-and-purity tr num days teams)
  (define unit-prop (unit-propogate tr))
  (begin
    (set! unit-prop (sch-binary-update tr unit-prop days teams))
    (cond [(equal? #f unit-prop) #f]
          [else (begin (set! tr (trim tr unit-prop))
                       (cond [(equal? #f tr) #f]
                             [(not (equal? #t tr)) (begin (define purity (purity-assigner tr num))
                                                          (merger unit-prop purity))]
                             [else unit-prop]))])))

