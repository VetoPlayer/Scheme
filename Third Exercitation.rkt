#lang racket


;; sum-list (propedeutical exercise for the fold)
;; named-let variant
(define (sum-list li)
  (let loop((accum 0)(l li))
    (if (null? l)
        accum
        (loop (+ accum (car l)) (cdr l)))))

;;TODO: Tail recursive version

;; fold left
;; i is actually used as an 'accum' variable
(define (my-foldl f i lst)
  (if (null? lst)
      i
      (my-foldl f (f (car lst) i) (cdr lst))))

;; Fold Right: since it begins computing the rigth-most element, a tail recursive version is not so trivial
;; Here i has a completely different semantic
(define (my-foldr f i lst)
  (if (null? lst)
      i
      (f(car lst) (my-foldr f i (cdr lst)))))
  
;;  sort takes a list to be sorted and an arbitrary ordering function f that returns true if the first argument is less than 
;; the second (return false to sort with a descending order)
(define (my-sort f lst)
  (define (min-list lst)
  (foldl (λ(x y)(if(f x y) x y)) (car lst) (cdr lst)))
  (if (null? lst)
      '()
      (let ((m (min-list lst)))
        (cons m (my-sort f (remove m lst))))))

; list< compares two numeric lists. returns true if the sum of the elements of the first list is less than the second. 
; if the two sums are equal, list< compares the length of the two lists.
;; todo: finish it!!
(define (list< l1 l2)
  (if (< (my-foldl (λ(x y)(+ x y)) (car l1) (cdr l1)) (my-foldl (λ(x y)(+ x y)) (car l2) (cdr l2)))
      #t
     #f ) )


;;exercise 1
;
;define the "gcd" function.
;
;gcd takes two numbers as input and computes the great commom divisor between the two.
; 
;
;examples:
;
;(gcd 8 28) -> 4
;(gcd 11 98) -> 1

(define (gcd n1 n2)
  (let ((all-numbers (sequence 1 (max n1 n2))))
    (maximum-lst (filter (λ(x)(and (equal? 0 (modulo n1 x)) (equal? 0 (modulo n2 x)))) all-numbers))))


(define (sequence lo hi)
  (cond ((> lo hi) (error "figa un errore"))
        ((< lo hi) (cons lo (sequence (+ 1 lo) hi)))
        (else (cons lo '()))))






;; Given a list of numebers, maximum returns the greater number of that list
;; Classic Version: it wants at least a list of 1 element

( define ( maximum x . rest )
   (if ( null? rest ) ; is rest = ()?
       x ; then return x
       ( apply maximum ; else : recursive call
               ( cons
                 (if (> x ( car rest ))
                     x
                     (car rest ))
                 (cdr rest )))))


;; Weird version that come into your mind

  
(define (maximum-lst lst)
  (let loop ((max (car lst)) (l lst))
    (if (null? l)
        max
        (loop (if (> max (car l)) max (car l)) (cdr l)
        ))

    

  ))






;; Ok let's do some STRUCTS!

;; Player struct
(struct player
  (name
   strength))

; (player "John" 60)
; (define john (player "John" 60))
; (player-name john)
; (player-strength john)
; (player? john)
; (set-player-name! john "New John") > error

(struct stricker player
  ((goals: #:mutable)))








