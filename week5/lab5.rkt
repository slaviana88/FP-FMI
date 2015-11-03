#lang racket
(define (sum2 numbers)
  (cond
    [(empty? numbers) 0]
    [else (+ (first numbers) (sum2 (rest numbers)))]
    ))

(define (length2 numbers)
  (define (iter numbers result)
    (cond
      [(empty? numbers) result]
      [else (iter (rest numbers) (+ result 1))]
      ))
  (iter numbers 0))

(define (member? str l)
  (cond
    [(empty? l) #f]
    [(equal? (first l) str) #t]
    [else (member? str (rest l))]
    ))

(define (list-ref2 l num)
  (define (iter l i)
  (cond
    [(equal? (first l) num) i]
    [else (iter (rest l) (+ i 1))]))
  (iter l 0))

(define (range2 a b)
  (cond
    [(> a b) (list )]
    [else (cons a (range2 (add1 a) b))]
   ))

(define (range-f f a b)
 (map f (range2 a b)))

(define (number-list n)
  (define (iter l n)
    (cond
      [(= (quotient n 10) 0) (cons n l)]
      [else (iter (add1 (quotient n 10)) (remainder n 10))]))
  (iter (list) n))

(define (map2 f xs)
  (cond
    [(empty? xs) (list)]
    [else (cons (f (first xs)) (map2 f (rest xs)))]))