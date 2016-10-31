#lang racket
;; Tommasini Exercitation remake by me
;; Ex 1: Hello World!!
(define (hello-world)
(display "Hello-World!!")
  )
;; Ex 2: Factorial
(define (factorial n)
  (if(= n 0)
     1 
     (* n (factorial (- n 1)))))
;; Ex 3: Reverse a pair
(define (reverse pair)
  (if (pair? pair)
      (let ((x (car pair)) (y (cdr pair)))  (cons y x))
     (error "Not a Pair!"))
  )

;; Ex 4: Sum of squares: take a list of numbers as its argument and returns the sum of the squares of the elements of the list.
(define (sum-of-squares list)
  (if (null? (cdr list))
      (* (car list)(car list))
  (+ (* (car list) (car list)) (sum-of-squares (cdr list)))))
;; Ex 5: Sum of Squares, Tail Recursive Version
;; In order to implement the tail recursion a further parameter has to be passed: an accumulator keeping the partial result
;; In the last call the accumulators keeps the final result! Keep it in Mind
;; The other, 'External' operations that have to be eliminated in order to implement the tail-recursive version are moved upon calling the procedure.
;; After having defined the tail recursive local procedure, you obviously need to call it the first time within the outer procedure definition
(define (tail-sum-of-squares list)
  (define (tailrec list acc)
    (if (null? list)
        acc
       (tailrec (cdr list) (+ acc (* (car list) (car list))))))
  (tailrec list 0))
;; Ex 6: Write a function returning a list of numbers included in a given range (high && low value are given as paramenters)
(define (range lo hi)
  (define (tailrange lo hi li)
  (if(= lo hi)
     li
     (tailrange (+ lo 1) hi (append li (list lo)))))
  (tailrange (+ 1 lo) hi '()))

;; Ex 7: Write a function returning how many numbers there are in the sequence of Fibonacci Numbers strictly lesser than the given argument
(define (fibonacci num)
  (define (fib-helper lo hi num acc)
    (if (> (+ lo hi) num)
        acc
        (fib-helper hi (+ lo hi) num (+ acc 1)))
    )
(fib-helper 0 1 num 2)
  )

;; Write a Function that taking as input a list, flattens it.
;; li = list
;; res = resulting list, conceptually is the same as an accumulator, it stores the partial and eventually the final result.
(define (flatten li)
  (define (tail-flatten li res)
    (if(null? li)
        res
        (if(list? (car li))
           (tail-flatten (cdr li) (tail-flatten (car li) res)) ;; Here you make a "Double" recursive call passing res because you want the procedure to add all the sublist element
           ;;Else you have to add to the current list a simple element and you make a simple recursive call:
           (tail-flatten (cdr li) (append res (list (car li)))))))
  (tail-flatten li '()))

;; Ex 8: My-Map: takes a function f and a list l and applies f to each elements of l; NB returns a list
(define (my-map funct li)
  (define (tail-my-map fu li acc)
    (if (null? li)
        acc
        (tail-my-map fu (cdr li) (append acc (list(fu (car li)))))))
  (tail-my-map funct li '()))

;; Ex 9: MyMap, non tail-recursive version
(define (myMap f l)
  (if(null? l)
  '()
  (cons (f (car l)) (myMap f (cdr l)))))
;; Ex 10: // TODO Let Defined with Map Excercise
  
  


