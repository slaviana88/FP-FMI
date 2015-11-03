#lang racket
(provide string-repeat)

(define (string-repeat str n)
  (define (helper result i)
    (if (= i n) result
        (helper (string-append result str) (+ i 1))))
  (helper "" 0))

(define (fence n)
  (define (helper result i)
    (cond
      [(= i n) (helper (string-append result "n") (+ i 1))]
      [(> i n) (string-append result (string-append (string-repeat "-" n) "}"))]
      [else (helper (string-append result (string-repeat "-" n)) (+ i 1))]
      ))
  (helper "{" (- n 1)))