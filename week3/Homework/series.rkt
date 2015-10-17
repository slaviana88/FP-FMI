#lang racket
(define (series a b n)
  (define (helper a b i)
    (cond
      [(= n 1) a]
      [(= n 2) b]
      [(> i n) b]
      [else (helper b (+ a b) (+ i 1))]))
  (helper a b 3))

(define (fibonacci n)
  (series 1 1 n))

(define (lucas n)
 (series 2 1 n))

(define (summed-member n)
  (+ (fibonacci n) (lucas n)))

(define (nth-series-sum series n)
  (define (helper i result)
    (cond
      [(> i n) result]
      [else (helper (+ i 1) (+ result (series i)))]))
  (helper 1 0))

(define (nth-lucas-sum n)
  (nth-series-sum lucas n))

(define (nth-fibonacci-sum n)
  (nth-series-sum fibonacci n))

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))
