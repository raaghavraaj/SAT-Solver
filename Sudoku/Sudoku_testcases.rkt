#lang racket
(provide test1
         test2
         test3
         test4)

(define test1 '#(#(#f #f #f  2  6 #f  7 #f  1)
                 #( 6  8 #f #f  7 #f #f  9 #f)
                 #( 1  9 #f #f #f  4  5 #f #f)
                 #( 8  2 #f  1 #f #f #f  4 #f)
                 #(#f #f  4  6 #f  2  9 #f #f)
                 #(#f  5 #f #f #f  3 #f  2  8)
                 #(#f #f  9  3 #f #f #f  7  4)
                 #(#f  4 #f #f  5 #f #f  3  6)
                 #( 7 #f  3 #f  1  8 #f #f #f)))

(define test2 '#(#( 9 #f  6 #f  7 #f  4 #f  3)
                 #(#f #f #f  4 #f #f  2 #f #f)
                 #(#f  7 #f #f  2  3 #f  1 #f)
                 #( 5 #f #f #f #f #f  1 #f #f)
                 #(#f  4 #f  2 #f  8 #f  6 #f)
                 #(#f #f  3 #f #f #f #f #f  5)
                 #(#f  3 #f  7 #f #f #f  5 #f)
                 #(#f #f  7 #f #f  5 #f #f #f)
                 #( 4 #f  5 #f  1 #f  7 #f  8)))


(define test3 '#(#(#f  2 #f  5 #f  1 #f  9 #f)
                 #( 8 #f #f  2 #f  3 #f #f  6)
                 #(#f  3 #f #f  6 #f #f  7 #f)
                 #(#f #f  1 #f #f #f  6 #f #f)
                 #( 5  4 #f #f #f #f #f  1  9)
                 #(#f #f  2 #f #f #f  7 #f #f)
                 #(#f  9 #f #f  3 #f #f  8 #f)
                 #( 2 #f #f  8 #f  4 #f #f  7)
                 #(#f  1 #f  9 #f  7 #f  6 #f)))

(define test4 '#(#(#f #f #f  #f  #f #f  #f #f #f)
                 #(#f #f #f  #f  #f #f  #f #f #f)
                 #(#f #f #f  #f  #f #f  #f #f #f)
                 #(#f #f #f  #f  #f #f  #f #f #f)
                 #(#f #f #f  #f  #f #f  #f #f #f)
                 #(#f  5 #f #f #f  3 #f  2  8)
                 #(#f #f  9  3 #f #f #f  7  4)
                 #(#f  4 #f #f  5 #f #f  3  6)
                 #( 7 #f  3 #f  1  8 #f #f #f)))
