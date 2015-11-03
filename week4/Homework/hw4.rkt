#lang racket
(require "../../week4/lab.rkt")
(require "../../week2/lab2.rkt")

(define (f p g h)
 (lambda (x) (and (p (g x)) (p (h x)))))

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (helper oper frac1 frac2)
   (/ ((get-operation oper) (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))

(define (add-frac frac1 frac2)
  (helper + frac1 frac2))

(define (substract-frac frac1 frac2)
  (helper - frac1 frac2))

(define (mult-frac frac1 frac2)
  (/ (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))))

