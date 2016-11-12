#lang racket
;; MACROS!!!!

(define-syntax swap
  (syntax-rules ()
    ((_ x y)
    (let ((temp x))
      (begin
        (set! x y)
        (set! y temp))))))

; repeat-until, a variant of the do-while construct
(define-syntax repeat
  (syntax-rules (until)
  ((_ body ... until cond)
   (let loop ()
     body ...
     (when cond
       (loop))))))
     