#lang racket

;I am going to rely on some of these methods from lecture for the later problems.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;START OF METHODS GIVEN DURING LECTURE.

(define G
  (lambda (i x y)
    (cond
      [(and (zero? y) (zero? i)) x]
      [(and (zero? y) (zero? (sub1 i))) 0]
      [(zero? y) 1]
      [(zero? i) (add1 (G i x (sub1 y)))]
      [else (G (sub1 i) x (G i x (sub1 y)))])))

;; now to show we have plus, times, ^ that we started with among
;; the infinitely others we define them

(define plus
  (lambda (x y)
    (G 0 x y)))

(define times
  (lambda (x y)
    (G 1 x y)))

(define ^
  (lambda (x y)
    (G 2 x y)))

;END OF METHODS GIVEN DURING LECTURE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Problem 1
(define countdown
  (lambda (n)
    (cond
      [(zero? n) (cons n '())]
      [else (cons n (countdown (sub1 n)))])))

;Problem 2
(define insertR
  (lambda (x y ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls))
       (cons x (cons y (insertR x y (cdr ls))))]
      [else (cons (car ls) (insertR x y (cdr ls)))])))

;Problem 3
(define remv-1st
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls)) (cdr ls)]
      [else (cons (car ls) (remv-1st x (cdr ls)))])))

;Problem 4
(define count-?s
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(eqv? '? (car ls))
       (add1 (count-?s (cdr ls)))]
      [else (count-?s (cdr ls))])))

;Problem 5
(define filter
  (lambda (pred ls)
    (cond
      [(null? ls) '()]
      [(pred (car ls))
       (cons (car ls) (filter pred (cdr ls)))]
      [else (filter pred (cdr ls))])))

;Problem 6
(define zip
  (lambda (ls1 ls2)
    (cond
      [(or (null? ls1) (null? ls2)) '()]
      [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))])))

;Problem 7
(define map
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [else (cons (proc (car ls)) (map proc (cdr ls)))])))

;Problem 8
(define append
  (lambda (ls1 ls2)
    (cond
      [(and (null? ls1) (null? ls2)) '()]
      [(null? ls1) (cons (car ls2) (append ls1 (cdr ls2)))]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

;Problem 9
(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (reverse (cdr ls)) `(,(car ls)))])))

;Problem 10
(define fact
  (lambda (n)
    (cond
      [(eqv? n 0) 1]
      [else (times n (fact (sub1 n)))])))

;Problem 11.
(define member-?*
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [(pair? (car ls)) (member-?* (car ls))]
      [else (or (eqv? '? (car ls)) (member-?* (cdr ls)))])))

;Problem 12
(define fib
  (lambda (n)
    (cond
      [(eqv? 0 n) 0]
      [(eqv? 1 n) 1]
      [else (plus (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

;Problem 13
;This passes (equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ()))) )
;My Answer: ((w . (x . ())) . (y . ((z . ()) . ())))

;Problem 14. Is this naturally recursive enough?
(define binary->natural
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(eqv? 0 (car ls)) (plus 0 (times 2 (binary->natural (cdr ls))))]
      [else (plus 1 (times 2 (binary->natural (cdr ls))))])))

;Problem 15
(define minus
  (lambda (x y)
    (cond
      [(eqv? 0 y) x]
      [else (minus (sub1 x) (sub1 y))])))

;Problem 16
;Helper function
(define div-help
  (lambda (i x y)
    (cond
      [(eqv? 0 x) i]
      [else (div-help (add1 i) (minus x y) y)])))
;End of Helper.

(define div
  (lambda (x y)
    (div-help 0 x y)))
    

;Problem 17
(define append-map
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [else (append (proc (car ls)) (append-map proc (cdr ls)))])))

;Problem 18
;Helper function
(define contains?
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eqv? x (car ls)) #t]
      [else (contains? x (cdr ls))])))
;End of helper function

(define set-difference
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [(null? ls1) '()]
      [(contains? (car ls1) ls2) (set-difference (cdr ls1) ls2)]
      [else (cons (car ls1) (set-difference (cdr ls1) ls2))])))
      