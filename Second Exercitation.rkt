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
        (loop(+ acc (*(car lk)(car lk)))(cdr lk)))))

;; My Map: Pay attention! list are immutable, my-map doesn't have side effects, the returned list is a new list based on
;; the evaluation of the lambda function

(define (my-map f li)
  (if (null? li)
      '()
      (cons (f (car li)) (my-map f (cdr li)))))

;; reverse map: accum is a lambda function taking ALWAYS! only a single input that is defined at every iteration.
;; reverse map applies the user defined lambda function over all the list but returns it as reversed:


(define (reverse-map func lst)
  (let loop ((l lst)(acc '()))
    (if (null? l)
        acc
    (loop (cdr lst) (cons (func (car lst)) acc)))))




;; Vector-length: counts the length of a vector and returns its length
;; That's how you parse a vector!

(define (vec-length vect)
  (let loop ((l (vector->list vect))(accum 0))
    (if (null? l)
        accum
        (loop (cdr l)(+ accum 1)))))


;; TODO: make an excercises with a cumulative lambda function that, within the recursive call, is redefined at each step.
;; Sequence returns a list of numbers between lo and hi included


(define (sequence lo hi)
  (cond ((> lo hi) (error "figa un errore"))
        ((< lo hi) (cons lo (sequence (+ 1 lo) hi)))
        (else (cons lo '()))))
;; final part of writing a list: cons ... '()

;;Sequence working with a Named Let Version!!
(define (named-sequence lo hi)
  (let loop ((l lo)(h hi))
    (cond ((> l h) (error "figa un errore"))
        ((< l h) (cons l (loop (+ 1 l) h)))
        (else (cons l '())))))

;; Inverse sequence: return a list of the specified sequence in the reversed order
(define (reverse-sequence lo hi)
  (let loop ((l lo)(h (+ 1 hi))(accum '()))
    (cond ((> l h) (error "figa un errore"))
        ((< l h) (loop (+ 1 l) h (cons l accum)))
        (else accum))))






;; vector-replace takes a mutable vector v and two values a and b. it replaces each occurence of a with b.
;; warning: you can't use this function with immutable vectors as literal vectors
;; It works, it just doesn't perform a print on the screen!
;; Try using these instructions: (vector-replace! v1 2 'a) (vector-replace! v1 2 'a)
(define (vector-replace! v a b)
  (for-each (lambda (pos)
              (when (equal? (vector-ref v pos) a)
              (vector-set! v pos b)))
            (sequence 0 (- (vector-length v) 1)))) ;; Here you are making a sequence because for-each wants as input parameter a list






;; easy example of for-each usage
(define (show lst)
  (for-each (lambda (x) (display x)) lst))

;; for each usage over vectors:
(define (show-vector vec)
  (for-each (lambda (x)(display x)) (vector->list vec)))


;;Let's make a vector-for-each!!!
;; body is the procedure you want ot apply over each array element

(define (vector-for-each body vect)
  (let ((max (- (vector-length vect) 1))) ;; Here you have initialized a variable BEFORE a Named Let!!
    (let loop ((i 0))
      (body (vector-ref vect i)) ;; vect[i]
      (when (< i max)
      (loop (+ i 1))))))







;; function making the power of 2 of the elements of a list: example usage of for-each
;(define (list-squared lst)
;  (for-each (lambda(x) ((* x x)(display x))) '(1 2 3)))


; odd-only takes an indefinite number of arguments and returns a list containg only the odd ones
;; from now on,  use lst for indicating a list as input parameter
;; odd? to check whether a number is odd



;; Another possible version is to build up the result list incrementally
(define (odd-only .lst)
  (cond ((null? .lst)'())
        ((odd? (car .lst)) (cons (car .lst) (odd-only (cdr .lst))))
        ((even? (car .lst)) (odd-only (cdr .lst))) ;; while building something, just skipping to the next recursive call is ok
      )
  )

;; TODO: Make the named Let Version







;; my-filter

(define (my-filter f lst)
  (if (null? lst)
      '()
      (if (f (car lst))
          (cons (car lst)(my-filter f (cdr lst)))
          (my-filter f (cdr lst)))))



;; reverse my filter
(define (reverse-my-filter f lst)
  (let loop ((l lst)(accum '()))
    (if (null? l)
        accum
        (if (f (car l))
            (loop (cdr l) (cons (car l) accum))
            (loop (cdr l) accum)))))




;;My solution: tail recursive
;; CM' ON GIMMIE A BREAK
 (define (tailrec-odd-only lst)
   (define (tail-rec-odd-only lst accum)
     (if(null? lst)
        (cons accum '())
        (if(odd? (car lst))
           (tail-rec-odd-only (cdr lst) (cons accum (car lst)))
           (tail-rec-odd-only (cdr lst) accum))))
   (tail-rec-odd-only lst '()))

;;TOFIX: It does the work the other way around

       
















