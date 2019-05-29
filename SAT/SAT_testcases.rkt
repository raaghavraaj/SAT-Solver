#lang racket
(require "../Utils/macros.rkt")
(provide test1
         test2
         test3
         test4
         test5
         test6
         test7
         test8
         test9
         test10
         test11
         test12
         test13
         test14)

(define test1 (node 'and (list
  (node 'not
                    (list (node 'and
                                (list(leaf 1)
                                     (leaf 2)))))
  (node 'not
        (list (node 'and
                    (list(leaf 3)
                         (leaf 2)))))
  (node 'not
        (list (node 'and
                    (list(leaf 1)
                         (leaf 3)))))
  (node 'or (list (leaf 1) (leaf 2) (leaf 3))))))

(define test2 (node 'or
                    (list (leaf 1)
                          (leaf 2))))

(define test3 (node 'and (list (leaf 4) (leaf 3))))

(define test4 (node 'or
                    (list (node 'not (list (leaf 1)))
                          (node 'not (list (leaf 2))))))

(define test5 (node 'or
                    (list (leaf 1)
                          (node 'and
                                (list (leaf 2)
                                      (leaf 3))))))

(define test6 (node 'and
                    (list (leaf 1)
                          (node 'or
                                (list (leaf 2)
                                      (leaf 3)))
                          (leaf 4))))

(define test7 (node 'and
                    (list (node 'or
                                (list (leaf 3)
                                      (leaf 4)))
                          (node 'not
                                (list (node 'or
                                            (list (leaf 1)
                                                  (leaf 6)
                                                  (node 'and
                                                        (list (leaf 2)
                                                              (leaf 5))))))))))

(define test8 (node 'not
                    (list (node 'or
                                (list (leaf 1)
                                      (leaf 4)
                                      (node 'and
                                            (list (leaf 2)
                                                  (leaf 3))))))))

(define test9 (node 'and
                    (list (leaf 1)
                          (node 'not
                                (list (leaf 1))))))

(define test10 (node
                'and
                (list
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 2)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 3)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 4)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 5)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 6)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 1) (leaf 9)))))

                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 3)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 4)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 5)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 6)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 2) (leaf 9)))))


                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 4)))))
                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 5)))))
                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 6)))))
                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 3) (leaf 9)))))

                 
                 (node 'not
                       (list (node 'and (list (leaf 4) (leaf 5)))))
                 (node 'not
                       (list (node 'and (list (leaf 4) (leaf 6)))))
                 (node 'not
                       (list (node 'and (list (leaf 4) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 4) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 4) (leaf 9)))))


                 (node 'not
                       (list (node 'and (list (leaf 5) (leaf 6)))))
                 (node 'not
                       (list (node 'and (list (leaf 5) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 5) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 5) (leaf 9)))))

                 
                 (node 'not
                       (list (node 'and (list (leaf 6) (leaf 7)))))
                 (node 'not
                       (list (node 'and (list (leaf 6) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 6) (leaf 9)))))

                 
                 (node 'not
                       (list (node 'and (list (leaf 7) (leaf 8)))))
                 (node 'not
                       (list (node 'and (list (leaf 7) (leaf 9)))))

                 
                 (node 'not
                       (list (node 'and (list (leaf 8) (leaf 9)))))                   
                  
                 (node 'or (list (leaf 1) (leaf 2) (leaf 3) (leaf 4) (leaf 5) (leaf 6) (leaf 7) (leaf 8) (leaf 9))))))

(define test11 (node
                'not
                (list
                 (node
                  'and
                  (list
                   (node 'or (list (leaf 2) (leaf 3) (leaf 4) (leaf 5) (leaf 6) (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 3) (leaf 4) (leaf 5) (leaf 6) (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 4) (leaf 5) (leaf 6) (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 5) (leaf 6) (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 6) (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 7) (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 8) (leaf 9)))
                   (node 'or (list (leaf 9))))))))

(define test12 (node 'or
                     (list (node 'and
                                 (list (leaf 1)
                                       (leaf 2)))
                           (node 'and
                                 (list (node 'not
                                             (list (leaf 3)))
                                       (leaf 4)))
                           (node 'and
                                 (list (node 'not
                                             (list (leaf 5)))
                                       (leaf 6))))))

(define test13 (node
                'and
                (list
                 (node
                  'not
                  (list
                   (node
                    'and
                    (list
                     (node 'or (list (leaf 2) (leaf 3) (leaf 4)))
                     (node 'or (list (leaf 1) (leaf 3) (leaf 4)))
                     (node 'or (list (leaf 2) (leaf 1) (leaf 4)))
                     (node 'or (list (leaf 2) (leaf 3) (leaf 1)))))))
                 (node 'or (list (leaf 1) (leaf 2) (leaf 3) (leaf 4))))))

(define test14
  (node
 'and
 (list
  (node
   'or
   (list
    (leaf 1)
    (leaf 2)
    (leaf 3)
    (leaf 4)
    (leaf 5)
    (leaf 6)
    (leaf 7)
    (leaf 8)
    (leaf 9)))
  (node
   'not
   (list
    (node
     'and
     (list
      (node
       'or
       (list
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 6)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 7)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 8)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 9)))
      (node
       'or
       (list
        (leaf 1)
        (leaf 2)
        (leaf 3)
        (leaf 4)
        (leaf 5)
        (leaf 6)
        (leaf 7)
        (leaf 8))))))))))