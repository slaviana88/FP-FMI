#lang racket
(provide
 sum-digits
 sum-divisors
 is-prime?)

(define (fact n)
 (define (fact-iter i result)
  ;(if (< n 2) 1
  ;   (* n (fact (- n 1)))) 
   (if (> i n) result
       (fact-iter ( + i 1) (* result i))))
  (fact-iter 1 1))
 
(define (sum-digits n)
;    (if (< n 10) n
;        (+ (remainder n 10) (sum-digits (quotient n 10)))))
    (define (sum-digits-iter n result)
      (if (< n 10) (+ n result)
          (sum-digits-iter (quotient n 10) (+ result (remainder n 10)))))
  (if (< n 0) (sum-digits-iter (- n) 0)
      (sum-digits-iter n 0))
 )

(define (fib n)
;  (if (< n 3) 1
;      (+ (fib (- n 1)) (fib (- n 2))))
  (define (fib-iter i a b)
    (if (= i n) (+ a b)
        (fib-iter (+ i 1) b (+ a b))))
  (fib-iter 3 1 1)
)

(define (sum-in-range a b)
  (define (sum-iter a result)
    (if (> a b) result
        (sum-iter (+ a 1) (+ a result))))
     (sum-iter a 0))

(define (sum-divisors n)
  (define (sum-divisors-iter i result)
    (cond
      [(> i n) result]
      [(integer? (/ n i)) (sum-divisors-iter (+ i 1) (+ result i))]
      [else (sum-divisors-iter (+ i  1) result)]
    ))
   (sum-divisors-iter 1 0))

(define (is-prime? n)
  (= (sum-divisors n) (+ n 1)))

(define (sum-primes-in-range a b)
  (define (helper a result)
    (cond
      [(> a b) result]
      [(is-prime? a) (helper (+ a 1) (+ result a))]
      [else (helper (+ a 1) result)]
      ))
  (helper a 0))

(define (sum-prime-divisors n)
  (define (helper i result)
    (cond
      [(> i n) result]
      [(is-prime? (/ n i)) (helper (+ i 1) (+ result i))]
      [else (helper (+ i  1) result)]
      ))
  (helper 1 0))


(define (reverse-int n)
  (define (helper n result)
    (if (= n 0) result
        (helper (quotient n 10) (+ (* result 10) (remainder n 10)))))
  (helper n 0))