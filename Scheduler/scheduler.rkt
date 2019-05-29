#lang racket
(require "../Utils/macros.rkt"
         "sat-solver-scheduler.rkt"
         "../Utils/to_sat_utils.rkt"
         "../Utils/to_cnf.rkt"
         "scheduler-utils.rkt")
;; A basic scheduler for league tournaments
;; Given N teams, returns a probable schedule with the following constraints
;; each team will play a two matches with another team, one home, one away
;; no team will have consecutive matches
;; other special constraints

;; Define a variable in the form D_T1_T2, which means on Day 'D'  Team T1 will play again Team T2, on T1's home ground
;; eg 030102 means on day 3, team 1 will play against team 2 on team 1's home ground.

;; Note that there will be NC2 days

;; initConstr is a list of pairs, or integers, or both
;; pairs of the form (cons d t) which would meant that team t must have a match on day d
;; ddt1t2 which would mean that on day d, t2 will face off t1 on t1's home
(provide scheduler)
(define (scheduler n [initConstr '()] [days #f])
  (begin
    (cond [(equal? days #f) (set! days (build-list (/ (* n (- n 1)) 2) (lambda (x) (+ 1 x))))]
          [else (set! days (build-list days (lambda (x) (+ 1 x))))])
    ;; basic definations
    (define teams (build-list n (lambda (x) (+ 1 x))))
 

    ;; node to ensure two matches of each team
    ;; ensures that each team plays one home and one away with a particular team
    (define two-match (make-node (lc (make-node (lc (leaf (to-term d t1 t2)) : d <- days) 'unique) : t1 <- teams t2 <- teams @(not (= t1 t2)))
                                 'and))

    ;; define the TL clause
    (define two-per-day (lc (cons 2 (lc (to-term d t1 t2) : t1 <- teams t2 <- teams @(not (= t1 t2)))) : d <- days))

    ;; node to ensure that a team does not have more than one match in any given two consecutive days
    ;; also ensures that a team does not have two matches on the same day as well
    (define max-one-in-two (make-node (lc (make-node (append* (lc (list (leaf (to-term d1 t1 t2))
                                                                        (leaf (to-term d1 t2 t1))
                                                                        (leaf (to-term d2 t1 t2))
                                                                        (leaf (to-term d2 t2 t1)))
                                                                  : t2 <- teams @(not (= t1 t2))))
                                                     'atmost-one)
                                          : d1 <- days d2 <- days @(= 1 (- d2 d1)) t1 <- teams)
                                      'and))

    ;; returns a node which exercises the constraint
    (define (constraint->node Cs)
      ;; (cons d t)
      ;; returns a node which ensures that team t will have a match on day d
      (define (pair-processor pr)
        (define t1 (cdr pr))
        (define d (car pr))
        (make-node (append* (lc (list (leaf (to-term d t1 t2)) (leaf (to-term d t2 t1))) : t2 <- teams @(not (= t2 t1))))
                   'unique))

      (define (int-process int) (leaf int))

      (node 'and (map (lambda (x) (if (pair? x) (pair-processor x) (int-process x))) Cs)))

    (define master (make-node (list two-match
                                    max-one-in-two
                                    (constraint->node initConstr)) 'and))

    (let ([res (sat-solver master 1000000 (make-hash) days teams two-per-day)])
      (if (not (hash? res)) #f (filter (lambda (x) (cdr x)) (hash->list res))))))

;(define test1 (scheduler 8 (map car (list (cons 10102 #t)
;                                          (cons 30301 #t)
;                                          (cons 60104 #t)
;                                          (cons 80501 #t)
;                                          (cons 100106 #t)
;                                          (cons 120107 #t)
;                                          (cons 140401 #t)
;                                          (cons 170601 #t)
;                                          (cons 190801 #t)
;                                          (cons 210201 #t)
;                                          (cons 240108 #t)
;                                          (cons 270105 #t)
;                                          (cons 300103 #t)
;                                          (cons 320701 #t))) 40))

(define (test2) (scheduler 6 (map car (list (cons 10102 #t)
                                          (cons 30301 #t)
                                          (cons 60104 #t)
                                          (cons 100106 #t)
                                          (cons 120401 #t)
                                          (cons 140601 #t)
                                          (cons 160201 #t)
                                          (cons 180105 #t)
                                          (cons 200103 #t))) 20))
                                          
;                                          (cons 10503 #t)
;                                          (cons 40205 #t)
;                                          (cons 60605 #t)
;                                          (cons 100805 #t)
;                                          (cons 120506 #t)
;                                          (cons 160504 #t)
;                                          (cons 180502 #t)
;                                          (cons 200305 #t)
;                                          (cons 230405 #t)
;                                          (cons 250705 #t)
;                                          (cons 290508 #t)
;                                          (cons 340507 #t))) 40))

