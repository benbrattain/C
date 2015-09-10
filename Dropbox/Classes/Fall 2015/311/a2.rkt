#lang racket
;ATTEMPTED PROBLEM 13 AND COULDNT SOLVE IT. WORK IS BELOW.
;PART 1

;Problem 1

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (cond
             [(eqv? 0 n) ls]
             [else (cdr (nth-cdr (sub1 n)))]))))
      (car (nth-cdr n)))))

;Problem 2
;Helper Function from HW 1
(define remv-1st
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls)) (cdr ls)]
      [else (cons (car ls) (remv-1st x (cdr ls)))])))
;End of Helper Function

;It will take the first element of ls1 and continually remove it from ls2 until there is none.
;Then it will move onto the next element of ls1, appending the 1st element, until we hit a null list somewhere. Then it appends both.
(define union
  (lambda (ls1 ls2)
    (cond
      [(or (null? ls1) (null? ls2)) (append ls1 ls2)]
      [(not (not (memv (car ls1) ls2))) (union ls1 (remv-1st (car (memv (car ls1) ls2)) ls2))]
      [else (append `(,(car ls1)) (union (cdr ls1) ls2))])))

;Problem 3

(define extend
  (lambda (n pred)
    (lambda (x)
      (or (eqv? n x) (pred x)))))

;Problem 4

(define walk-symbol
  (lambda (x ls)
    (letrec ([walk-through
              (lambda (x s)
                (cond
                  [(or (not (symbol? x)) (null? s)) x]
                  [(eqv? x (car (car s))) (walk-symbol (cdr (car s)) ls)]
                  [else (walk-through x (cdr s))]))])
    (walk-through x ls))))

;PART 2

;Problem 5

(define lambda->lumbda
  (lambda (expr)
    (match expr
      [`,x #:when(symbol? x) x]
      [`(lambda (,x) ,f) (append (append `(lumbda) `((,x))) (list `,(lambda->lumbda f)))]
      [`(,rator ,rand) (append (list `,(lambda->lumbda rator)) (list `,(lambda->lumbda rand)))])))

;Problem 6
(define var-occurs?
  (lambda (y expr)
    (match expr
      [`,x #:when(symbol? x) (eqv? x y)]
      [`(lambda(,x) ,f) `,(var-occurs? y f)]
      [`(,rator ,rand) `,(or (var-occurs? y rator) (var-occurs? y rand))])))

;Problem 7
(define vars
  (lambda (expr)
    (match expr
      [`,x #:when(symbol? x) (list x)]
      [`(lambda (,x) ,f) (vars f)]
      [`(,rator ,rand) (append `,(vars rator)`,(vars rand))])))

;Problem 8
(define unique-vars
  (lambda (expr)
    (match expr
      [`,x #:when(symbol? x) (list x)]
      [`(lambda (,x) ,f) (unique-vars f)]
      [`(,rator ,rand) (union `,(unique-vars rator)`,(unique-vars rand))])))

;Problem 9
(define var-occurs-free?
  (lambda (x expr)
    (and (var-occurs? x expr)
         (match expr
           [`,y #:when(symbol? y) (eqv? y x)]
           [`(lambda (,y) ,f) (and (not (eqv? x y)) (var-occurs? x f))]
           [`(,rator ,rand) (or `,(var-occurs-free? x rator) `,(var-occurs-free? x rand))]))))

;Problem 10
(define var-occurs-bound?
  (lambda (x expr)
    (and (var-occurs? x expr)
         (match expr
           [`,y #:when(symbol? y) (not (eqv? y x))]
           [`(lambda (,y) ,f) (or (and (eqv? y x) (var-occurs? x f)) `,(var-occurs-bound? x f))]
           [`(,rator ,rand) (or `,(var-occurs-bound? x rator) `,(var-occurs-bound? x rand))]))))

;Problem 11
(define unique-free-vars
  (lambda (expr)
    (match expr
      [`,x #:when(symbol? x) (list x)]
      [`(lambda (,x) ,f) (remove x (unique-free-vars f))]
      [`(,rator ,rand) (union `,(unique-free-vars rator)`,(unique-free-vars rand))])))

;Problem 12
(define unique-bound-vars
  (lambda (expr)
    (match expr
      [`,x #:when(symbol? x) '()]
      [`(lambda (,x) ,f) (if (var-occurs-bound? x expr) (append `(,x) (unique-bound-vars f)) (unique-bound-vars f))]
      [`(,rator ,rand) (union `,(unique-bound-vars rator)`,(unique-bound-vars rand))])))

;Problem 13
;(define lex
;  (lambda (expr env)
;    (match expr
;      [`,x #:when(symbol? x) (append `(var ,x) env)]
;      [`(lambda (,x) ,f) (let [y 0] (cons 'lambda (add1 (lex f env))))]
;      [`(,rator ,rand) `(,(lex rator env) ,(lex rand env))])))
;I couldn't solve this, I tried a similar method to rebruinizer, but hit a snag. I wanted to set a variable to 0 and add 1 until I hit the variable.
;Then I would append it to my env.
  