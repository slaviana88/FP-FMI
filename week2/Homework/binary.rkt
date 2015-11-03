#lang racket
(define (string-reverse str)
  (define (helper len result)
    (cond
      [(= len 0) result]
      [else (helper (- len 1) (string-append result (~a (string-ref str (- len 1)))))]
      ))
  (helper (string-length str) ""))

(define (to-binary-string n)
  (define (helper result n)
    (cond
      [(= n 0) (string-reverse result)]
      [else (helper (string-append result (~a (remainder n 2))) (quotient n 2))]))
 (helper "" n))

(define (from-binary-string binary-str)
  (define (helper result n i)
    (cond
      [(< (string-length binary-str) i) result]
      [(= (remainder n 10) 1) (helper (+ result (expt 2 i)) (quotient n 10) (+ i 1))]
      [else (helper result (quotient n 10) (+ i 1))]
))
  (helper 0 (string->number binary-str) 0))