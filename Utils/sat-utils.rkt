#lang racket
(require "macros.rkt")

;; Utils file for the main sat solver
;; contains all the helper functions to solve a given tree

;; returns a hash-table with all unit clauses filled
;; returns false if contracting unit clauses found
;; expects a tree in cnf format
(provide unit-propogate)
(define (unit-propogate tree)
  ;; adds a cons pair to the existing list.
  ;; if already exists, then does nothing
  ;; if contradicting case arises, returns false
  (define (adder var bool lst)
    (let ([res (assoc var lst)])
      (cond [(equal? #f res) (cons (cons var bool) lst)]
            [(not (equal? bool (cdr res))) #f]
            [else lst])))

  ;; helper to iterate over the list of branches of the main tree to find unit-propogable clauses
  ;; returns a list of pairs or #f
  (define (helper lst ans)
    (match lst
      ['() ans]
      [(cons a rest) (match a
                       [(leaf val) (cond [(> val 0) (let ([res (adder val #t ans)])
                                                      (cond [(equal? #f res) #f]
                                                            [else (helper (cdr lst) res)]))]
                                         [else (let ([res (adder (- val) #f ans)])
                                                 (cond [(equal? #f res) #f]
                                                       [else (helper (cdr lst) res)]))])]
                       [else (helper (cdr lst) ans)])]))

  ;; main body
  (let ([bindings (cond [(leaf? tree) (helper (list tree) '())]
                        [else (helper (node-list tree) '())])])
    (if (equal? #f bindings) #f (make-hash bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END UNIT PROPOGATE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a complete SAT solution in the form of list of hash-tables using given bindings
;;;(define (completer hash-tbl num)
;;;  (define (helper i)
;;;    (cond [(> i num) '()]
;;;          [(hash-has-key? hash-tbl i) (cons (list (cons i (hash-ref hash-tbl i))) (helper (+ 1 i)))]
;;;          [else (cons (list (cons i #t) (cons i #f)) (helper (+ 1 i)))]))
;;;  (define (op x y)
;;;    (lc (cons a b) : a <- x b <- y))
;;;  (map make-hash (foldl op '(()) (helper 1))))
(provide completer)
(define (completer hash-tbl num)
  (define (helper i)
    (cond [(> i num) '()]
          [(hash-has-key? hash-tbl i) (cons (list (cons i (hash-ref hash-tbl i))) (helper (+ 1 i)))]
          [else (cons (list (cons i #f)) (helper (+ 1 i)))]))
  (define (op x y)
    (lc (cons a b) : a <- x b <- y))
  (car (map make-hash (foldl op '(()) (helper 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END COMPLETER ;;;;;;;;;;;;;;;;;;;;;;;

;; checks all literals for their purity in all clauses
;; returns all possible hash-table containing proper (sufficient) bindings for pure literals
;; assumes the tree to be in cnf format
(provide purity-assigner)
(define (purity-assigner tree num)
  ;; key for any variable : 
  ;; 'undefined -> variable not yet found
  ;; 'mud-blood -> not a pure literal
  ;; #t -> all instances till now are pure blooded and +ve
  ;; #f -> all instances till now are pure blooded and -ve
  (define ans-table (make-hash (build-list num (lambda (x) (cons (+ 1 x) 'undefined)))))

  ;; updates a given leaf value in the ans-table
  (define (updater leaf-val)
    (cond [(positive? leaf-val) (let ([store (hash-ref ans-table leaf-val)])
                                  (cond [(equal? store 'undefined) (hash-set! ans-table leaf-val #t)]
                                        [(equal? store 'mudblood) (void)]
                                        [(equal? store #f) (hash-set! ans-table leaf-val 'mudblood)]
                                        [(equal? store #t) (void)]))]
          [else (let ([store (hash-ref ans-table (- leaf-val))])
                  (cond [(equal? store 'undefined) (hash-set! ans-table (- leaf-val) #f)]
                        [(equal? store 'mudblood) (void)]
                        [(equal? store #t) (hash-set! ans-table (- leaf-val) 'mudblood)]
                        [(equal? store #f) (void)]))]))
  
  ;; iterates over all variables of a tree and updates them into the ans-table
  (define (iterate tr)
    (match tr
      [(leaf val) (updater val)]
      [(node op lst) (begin (map iterate lst)
                            (void))]))

  ;; main body of purity assigner
  (begin (iterate tree)
         (define pure-list (filter-not (lambda (x) (or (equal? (cdr x) 'undefined)
                                                       (equal? (cdr x) 'mudblood))) (hash->list ans-table)))
         (make-hash pure-list)))
  

;;;  ;; returns whether a tree has a given variable
;;;  (define (tree-has-variable? variable tree)
;;;    (match tree
;;;      [(leaf val) (= (abs val) variable)]
;;;      [(node op lst) (foldl (lambda (x y) (if (equal? y #t) #t (or y (tree-has-variable? variable x)))) #f lst)]))
  
;; takes input as a list of pairs of pure variables
;; returns all possible solutions using given purity variables
;;;  (define (solution-maker soln-list tree)
;;;    (cond [(null? soln-list) '(())]
;;;          [else (map (lambda (x) (cond [(tree-has-variable? (car x) tree) (let ([new-tree (trim tree (make-hash(list x)))])
;;;                                                                            (cond [(equal? #t new-tree) (list x)]
;;;                                                                                  [else (lc (cons x a) : a <- (solution-maker (filter (lambda (x) (tree-has-variable? (car x) new-tree)) soln-list)
;;;                                                                                                                         new-tree))]))]
;;;                                       [else (solution-maker (remove x soln-list) tree)])) soln-list)]))

;;;  ;; main body of purity assigner
;;;  (begin (iterate tree)
;;;         (define pure-list (filter-not (lambda (x) (or (equal? (cdr x) 'undefined)
;;;                                                       (equal? (cdr x) 'mudblood))) (hash->list ans-table)))
;;;         (map make-hash (remove-duplicates (solution-maker pure-list tree)))))
;;;         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END PURITY CHECKER;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; merges a purity checker hash-table into our unit-propogator solution
;; in case of contrasting solutions, unit-propogator will get preference
(provide merger)
(define (merger unit-prop purity)
  (define (adder pair ans-table)
    (cond [(hash-has-key? ans-table (car pair)) (void)]
          [else (hash-set! ans-table (car pair) (cdr pair))]))
  (begin (map (lambda (x) (adder x unit-prop)) (hash->list purity))
         unit-prop))

(provide strict-merger)
(define (strict-merger merge-into merge-this)
  (cond [(or (equal? #f merge-this) (equal? #f merge-into)) #f]
        [else
         (define ans-hash merge-into)
         (define (adder lst ans-table)
           (cond [(null? lst) ans-table]
                 [(hash-has-key? ans-table (caar lst)) (if (equal? (cdar lst) (hash-ref ans-table (caar lst))) (adder (cdr lst) ans-table)
                                                           #f)]
                 [else (begin
                         (adder (cdr lst) (make-hash (cons (cons (caar lst) (cdar lst)) (hash->list ans-table)))))]))
         (adder (hash->list merge-this) ans-hash)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END MERGER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a final hash-table after unit-propogation and purity check
;; assumes the tree to be in cnf format
(provide hash-after-unit-prop-binary-and-purity)
(define (hash-after-unit-prop-binary-and-purity tr num)
  (define unit-prop (unit-propogate tr))
  (begin
    (displayln (list "a" unit-prop)) 
    (set! unit-prop (binary-update tr unit-prop))
    (cond [(equal? #f unit-prop) #f]
          [else (begin (set! tr (trim tr unit-prop))
                       (cond [(equal? #f tr) #f]
                             [(not (equal? #t tr)) (begin (define purity (purity-assigner tr num))
                                                          (merger unit-prop purity))]
                             [else unit-prop]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END HASH-AFTER_UNIT_PROP_AND_PURITY ;;;;;;;;;;;;;;;

;; trims a tree!
(provide trim)
(define (trim tree ans-tbl)
  ;; returns the leaf if the variable is not present in ans-tbl
  ;; returns #t if the evaluated value is #t , else #f
  (define (leaf->val lf)
    (if (hash-has-key? ans-tbl (abs (leaf-val lf))) (let ([bool (hash-ref ans-tbl (abs (leaf-val lf)))])
                                                      (if (positive? (leaf-val lf)) bool (not bool)))
        lf))
  (if (equal? #f ans-tbl) #f
      (match tree
        [#f #f]
        [#t #t]
        [(leaf val) (leaf->val tree)]
        [(node 'and lst) (let ([res (map (lambda (x) (trim x ans-tbl)) lst)])
                           (if (list? (member #f res)) #f
                               (node 'and (remove* '(#t) res))))]
        [(node 'or lst) (let ([res (map (lambda (x) (trim x ans-tbl)) lst)])
                          (if (list? (member #t res)) #t
                              (node 'or (remove* '(#f) res))))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TRIM :::::::::::::::::::::::::::::::::::::::

;; returns a unique variable not in ans-tbl
;; if no unique variable exists, then returns 'none
(provide return-first-variable)
(define (return-first-variable tr ans-tbl)
  ;; returns the first variable found
  (define (helper lst)
    (cond [(null? lst) 'none]
          [else (let* ([first (car lst)]
                       [var (return-first-variable first ans-tbl)])
                  (cond [(number? var) var]
                        [else (helper (cdr lst))]))]))
  (define (unique? var)
    (not (hash-has-key? ans-tbl var)))
  (match tr
    [#f #f]
    [(leaf val) (if (unique? (abs val)) (abs val) 'none)]
    [(node op lst) (helper (shuffle lst))]))
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END RETURN VARIABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a complemnt result of a given answer
(provide complement)
(define (complement hash-lst)
  (define (helper ans-hash)
    (let ([ans-lst (hash->list ans-hash)])
      (define (op x y)
        (cond [(equal? (cdr x) #t) (cons (cons (car x) #f) y)]
              [else (cons (cons (car x) #t) y)]))
      (foldr op '() ans-lst)))
  (map helper hash-lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END COMPLEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; valid only for an 'or node
;; checks every binary clause
;; if any one of them is false, then the otherone must be true
;; returns void
;; updates ans-tbl directly
;; returns #f if conflicting cases found
;; returns #f if both clauses are false
;; returns void if no clauses found or if one is found and is true
;; tr has to be in cnf format
(provide binary-update)
(define (binary-update tr ans-tbl)
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
                                            (if (equal? #f res) #f (strict-merger (sudoku-hash-completer res) ans-tbl)))]))])]))


;; takes an input and completes the hash
;; returns a completed hash table
(provide sudoku-hash-completer)
(define (sudoku-hash-completer hash)
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
  (define (row-extender elem)
    (define row-iter (+ (* 100 (quotient (car elem) 100)) (remainder (car elem) 10)))
    (if (cdr elem) (remove (cons (car elem) #f) (build-list 9 (lambda (x) (cons (+ row-iter (* 10 (+ 1 x))) #f))))
        (list elem)))

  (define (column-extender elem)
    (define column-iter (remainder (car elem) 100))
    (if (cdr elem) (remove (cons (car elem) #f) (build-list 9 (lambda (x) (cons (+ column-iter (* 100 (+ 1 x))) #f))))
        (list elem)))

  (define (3x3-extender elem)
    (if (not (cdr elem)) (list elem)
        (let* ([val (car elem)]
               [xgrad (quotient (- val 100) 300)]
               [ygrad (quotient (- (remainder (quotient val 10) 10) 1) 3)]
               [value (remainder val 10)]
               [lst (lc (cons (+ (* 100 (+ a (* 3 xgrad)))
                                 (* 10 (+ b (* 3 ygrad)))
                                 value) #f) : a <- '(1 2 3) b <- '(1 2 3))])
          (remove (cons (car elem) #f) lst))))
          

  (make-hash (append
              (remove* (list (void)) (append* (map extender (hash->list hash))))
              (append* (map row-extender (hash->list hash)))
              (append* (map 3x3-extender (hash->list hash)))
              (append* (map column-extender (hash->list hash))))))


;; evaluates a given tree using a ans-tbl
;;  returns either #t, #f, 'not
(provide eval-tr)
(define (eval-tr tr ans-tbl)
  ;; evaluates a variable
  (define (eval-var val)
    (cond [(hash-has-key? ans-tbl (abs val)) (if (positive? val) (hash-ref ans-tbl val) (not (hash-ref ans-tbl (- val))))]
          [else 'not]))
  ;; evaluates an and expression
  (define (eval-and nd)
    (define lst (map (lambda (x) (eval-tr x ans-tbl)) (node-list nd)))
    (cond [(list? (member #f lst)) #f]
          [(list? (member 'not lst)) 'not]
          [else #t]))
  ;; evaluates an or expression
  (define (eval-or nd)
    (define lst (map (lambda (x) (eval-tr x ans-tbl)) (node-list nd)))
    (cond [(list? (member #t lst)) #t]
          [(list? (member 'not lst)) 'not]
          [else #f]))

  ;; main body
  (match tr
    [#t #t]
    [#f #f]
    [(leaf val) (eval-var val)]
    [(node 'and lst) (eval-and tr)]
    [(node 'or lst) (eval-or tr)]))