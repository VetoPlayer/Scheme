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

;; Named Let: Introduction
;; The named let contruct lets you define a sort of procedure that is recursive in the definition itself.

(define (named-let) (let ((x 0)) ;; Typical, normal use of the let construct
  (let label () ;; NAMED LET construct!! I think it's like a function that doesn't take any arguments as input
    (when (< x 10)
      (display x)
      (newline)
      (set! x (+ x 1))
      (label))))) ;; You recall your own definition, it's like a function call, a //GOTO

;; A more correct, idiomatic way of doing the same thing is the following:

(define (named-let-second)
  (let label ((x 0))
    (when (< x 10)
      (display x)
      (newline)
      (label (+ x 1)))))


;; Ex 10: Let Defined with Map Excercise

(define (named-let-my-map fu li)
  (let helper ((ff fu) (ll li))
    (if (null? ll)
        '()
        (cons (ff (car ll)) (helper fu (cdr ll))))))

;; Ex 11: checks if a number is Prime
(define (prime? n)
  (if (< n 3)
      #t
      (let label ((i 2))
        (if (= i n)
            #t
            (if (= (modulo n i) 0)
                #f
                (label (+ i 1)))))))

;; Ex 12: QuickSort algorithm: p = pivot element
(define (my-quicksort arr)
  (if (or (empty? arr) (empty? (cdr arr)))
      arr
      (let* ((p (car arr))
             (s< (filter (lambda(x)(< x p)) (cdr arr)))
             (s> (filter (lambda(x)(> x p)) (cdr arr))))
             (append (my-quicksort s<) (list p) (my-quicksort s>)))))
  
  


