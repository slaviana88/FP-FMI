#lang racket
(define (square x)
  (* x x))

(define (circle? circle-x circle-y radius point-x point-y)
 (<= (+ (square (- point-x circle-x)) (square (- point-y circle-y))) (square radius)))

(define (semiperimeter a b c)
  (/ (+ a b c) 2))

(define (area a b c)
  (sqrt (* (semiperimeter a b c) (- (semiperimeter a b c) a) (- (semiperimeter a b c) b) (- (semiperimeter a b c) c))))

(define (prime? n)
  (cond
   [(or (= n 2)
        (= n 3)
        (= n 5)
        (= n 7)) #t]
   [(integer?
     (or
      (/ n 2)
      (/ n 3)
      (/ n 5)
      (/ n 7))) #f]
   [else #t]))

(define (cube-sums n a)
  (cond
    [(> a (/ n 2)) #f]
    [(and
      (integer? (expt a (/ 1 3)))
       (integer? (expt (- n a) (/ 1 3)))) #t]
    [else (cube-sums n (+ a 1))]))

(define (cube-sums? n)
  (cube-sums n 1))

