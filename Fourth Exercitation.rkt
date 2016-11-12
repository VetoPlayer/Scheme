#lang racket
;; Closures 


;; 1] Define an adder
;; NB: n represents the "settable" parameter that is setted upon the first closure call.
(define (adder-n n)
  (λ(x)(+ x n)))


;;2] Define an iterator over a vector
;; NB: The parameter you give to the λ function is the one that has to be called the ordinary calls after the first one!!
;; All the computation part needs to be inside the λ!!!
;; All the initialization part needs to be outside the λ!!
(define (make-iterator vect)
  (let ((pos 0)(length (vector-length vect)))
    (λ()(if(= pos length)
            '<<End>>
            (let ((v (vector-ref vect pos)))
              (set! pos (+ pos 1))
              v ))))) ;; Do you need to perfom things in a sequential way and you don't want to use begin??


;; Continuations!! 

;;1] Break Example:
;; p is a predicate function
;; lst is the input list

(define (break-test p lst)
  (call/cc (λ(break)
             (for-each(λ(i) ;; i in the for-each contrusct is th i-th element passed to the lambda function so far. λ will be called with all i
                        (if(p i)
                           (break)
                        (begin
                          (display i)
                          (newline)))) lst) ;; Obviously the for-each contruct require a list of elements to iterate over
              ))

(display "End")) 

;;2] A "simple example" from theory lesson: remake
;; Remark: in this excercise you don't explicitly call k while executing test-cunt.
;; Yo Bitches!

(define saved-cunt #f)

(define (test-cunt)
  (let ((x 0))
    (call/cc (λ(k) ;; k, at every step, contains the continuation
               (set! saved-cunt k)))
    (set! x (+ x 1))
    (display x)
    (newline)))

;; Is in this case the continuation "stored" only after the set!, display and newline instructions?? probably YES

;; (test-cunt)--> 1
;; (saved-cunt)-->2

;; Remark: saved-cunt before the call of test-cunt is only a variable. Then it becomes a function



;; Interesting things: how to build up a λ function step by step
;; 1] How to force foldr to be tail-recursive
(define (fold-right-tail f i lst)
  (define (tail-helper-function f i lst out) ;; out is the usua "accumulator" parameter, but the interesting thing is that in this case is a procedure!
    (if (null? lst)
        out ;; If the list is null, we call the procedure out that performs the whole computation
        (tail-helper-function f i (cdr lst) (λ(x)(out (f(car lst) x)))) ;; remark: f takes 2 parameters!!!! the current element and an x that will be defined in the future
        ))
  (tail-helper-function f i lst (λ(x)(x))))

;; OUT CHANGESSSSSSSSSSS but stores the variable: is a closure!!
;; out is a changing procedure!! At each iteration it is redefined:
;; YOu define your new function as the application of the old (current function): as settable parameter you give the function f defined over (car lst) and an unknown one, the x, which will be given when you stop defining the function
;;1]out = Identity (x) ---> (λ(x)(x))
;;2]out = Identity (f e1 x) ---> (f 1 x)
;; 3] (f 1 (f 2 x)))
;; This thing here is a Big, Big closure!!



             