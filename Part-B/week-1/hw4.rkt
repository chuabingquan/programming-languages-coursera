
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Q1.
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Q2.
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Q3.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

; Q4.
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

; Q5.
(define (funny-number-stream)
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- 0 x)
                          x)
                      (lambda () (f (+ x 1))))
                )])
    (f 1)))

; Q6.
(define (dan-then-dog)
  (letrec ([f (lambda (x y) (cons x (lambda () (f y x))))])
    (f "dan.jpg" "dog.jpg")))

; Q7.
(define (stream-add-zero s)
  (lambda () (let ([pair (s)])
               (cons (cons 0 (car pair)) (stream-add-zero (cdr pair))))))