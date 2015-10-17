#lang racket
(define (string-reverse str)
  (define (helper len result)
    (cond
      [(= len 0) result]
      [else (helper (- len 1) (string-append result (~a (string-ref str (- len 1)))))]
      ))
  (helper (string-length str) ""))

(define (palindrome? str)
  (equal? str (string-reverse str)))

(define (sum-divisors n)
  (define (sum-iter i result)
    (cond [(= i n) result]
          [(= (remainder n i) 0) (sum-iter (+ i 1) (+ result i))]
          [else (sum-iter (+ i 1) result)]))
  (sum-iter 1 0))

(define (perfect? n)
  (= n (sum-divisors n )))

(define (occurences a n)
  (define (helper counter n)
    (cond
      [(= (quotient n 10) 0) counter]
      [(= a (remainder n 10)) (helper (+ counter 1) (quotient n 10))]
      [else (helper counter (quotient n 10))]))
  (helper 0 n))

(define (increasing? n)
    (cond
      [(< (remainder n 10) (remainder (quotient n 10) 10)) #f]
      [(= (quotient n 10) 0) #t]
      [else (increasing? (quotient n 10))]))

(define (occurences? a n)
    (cond
      [(= a (remainder n 10)) #t]
      [(= (quotient n 10) 0) #f]
      [else (occurences? a (quotient n 10))]))

(define (contains-digits? y x)
 (cond
  [(< y 10) (occurences? y x)] 
  [(occurences? (remainder y 10) x) (contains-digits? (quotient y 10) x)]
  [else #f]))
        