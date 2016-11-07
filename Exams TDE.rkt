#lang racket
;;5/07/2013 function numberList taking as input a list of numbers and returns a list of pairs of numbers: yJ= xi + xJ
;; li: input list, second parameter: xi of the previous iteration res: result
(define (numberList li)
  (define (tail-numberList li xi res)
    (if (null? li)
        res
       (tail-numberList (cdr li) (+ xi (car li)) (list res (cons (car li) (+ xi (car li)))))))
  (tail-numberList li 0 '()))

;;