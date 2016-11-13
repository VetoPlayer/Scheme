#lang racket
;;5/07/2013 function numberList taking as input a list of numbers and returns a list of pairs of numbers: xJ=xJ yJ= xi + xJ
;; li: input list, second parameter: xi of the previous iteration res: result

;; Remake: It's way easier than it seems

(define (numberList lst state)
  (if (null? lst)
      '()
      (cons (cons (car lst) (+ (car lst) state)) (numberList (cdr lst) (+ state (car lst))))))

;; 24/07/2013
;; Define a double linked list that has 3 components: 1° prev pointer 2° datum 3° next pointer.
;; A well made list has its first element's prev and its last element's next set to NIL

;; Define DList Datatype
(struct Dlist (
  (prev #:mutable)
  (datum #:mutable)
  (next #:mutable)
  ))
;; Yes, prev and next are pointers

;; Nil Object
;; HERE Nil is implicitly defined also as a constructor!
;; If you want to build up a Nil element for a Dlist you shoul invoke the constructor  instead of doing like (Dlist #f #f #f)
;; because in that case the eq? operator for Nil will evaluate false
(define Nil (Dlist #f #f #f))
(define (Nil? x) (eq? x Nil))

;; Remark: the whole list is passed as its first node!! This has the links to all the nodes within the list


;; Dcar
(define (Dcar node)
  (if (Nil? node)
      (error "Dcar of Nil")
      (Dlist-datum node)))

;; Dcdr
(define (Dcdr node)
  (if (Nil? node)
      (error "Dcdr of a Nil, you bastard")
      (let ((next (Dlist-next node)))
        (Dlist Nil (Dlist-datum next) (Dlist-next next)))))

;; Dcons
(define (Dcons item node)
  (if (Nil? node)
      (Dlist Nil item Nil)
      (let* ((newnode (Dlist Nil item (Dlist-datum node)))
             (newcar (Dlist Nil (Dlist-datum node) (Dlist-next node))))
        (set-Dlist-prev! newcar newnode)) ;; Here you set up the pointer, you need to fully create it in the let* assignment

      ))

;; Dlist? holding if both elements are equal
;; Making it recursive it the smart thing to do ;)
(define (Dlist=? node1 node2)
  (cond ((and (Nil? node1)(Nil? node2)) #t)
        ((and (Nil? node1)(Nil? node2)) #f)
        (else
         (equal? (Dlist-datum node1) (Dlist-datum node2))
         (Dlist=? (Dcdr node1) (Dcdr node2 )))))


;; 05/09/2013 Parent-pointer Trees
; You don't need a struct, you already have a vector of pairs for representing everything
;(struct PPT ((parent #:mutable)
;            (data #:mutable)))

;; define the operation find-root, to obtain the root of the tree containing the given node
;; Remark: node is a pair
(define (find-root vect node)
  (if (Nothing? (car node))
       node
      (find-root (vect) (vector-ref vect (cdr node)))))

(define (Nothing? var)
  (eq? #\? var))

;; define union! that takes 2 nodes and, if they belong to different trees, merges the 2 trees making the first root as a parent of the second one
;; remark: obviously it has side effects!
(define (union! vect node1 node2)
  (let((root1 (find-root node1)) (root2 (find-root node2)))
    (if (eq? root1 root2)
        (let ((root2parent (car root2)))
        (set! root2parent root1)) ;; To fix
        (display "The 2 selected nodes belong to the same tree"))))

;; REDO with indexes

;; 19/09/2013
;; Closure: If you want to initialize some variables by yourself you can easily do it by a let procedure

;; Closure as Objects!!
(define (make-object lst)
  (let ((my-lst lst))
     (define (member? my-lst elem) ;; Tail recursive because I do want to do so :P
      (if (null? my-lst)
          #f
          (if (= (car my-lst) elem)
              #t
              (or #f (member? (cdr my-lst) elem))
          )


      ))







    ;; Dispatcher
  (λ(message . args)
    (apply (case message
         ((member?) member? )
         ((subsetsum?) subsetsum?)
             (else (error "Method not available"))



         ) args ))))








