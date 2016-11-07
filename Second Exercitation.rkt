#lang racket
;; Excercitation 2
;;1) Implement Sum-Of-Squares with named loop: Really low level version

(define (sum-of-squares li)
  (define acc 0)
  (define lk li)
  (let loop()
    (if(null? lk)
       acc
       (begin
       (set! acc (+ acc (* (car lk)(car lk))))
       (set! lk (cdr lk))
       (loop)))))

;; 1.a) More interesting version:

(define (interesting-sum-of-square li)
  (let loop((acc 0)(lk li))
    (if (null? lk)
        acc
        (loop(+ acc (*(car lk)(car lk)))(cdr lk)))


    )


  )

