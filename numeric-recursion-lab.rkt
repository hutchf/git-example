#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   numeric-recursion-lab.scm
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   YOUR NAME HERE
;;; Summary:
;;;   Code for the lab entitled "Numeric Recursion"

;;; Procedure:
;;;   factorial
;;; Parameters:
;;;   n, a positive integer
;;; Purpose:
;;;   Compute n!, the product of positive integers less than or
;;;   equal to a given positive integer.
;;; Produces:
;;;   product, an integer
;;; Preconditions:
;;;   n is a number, exact, an integer, and positive.
;;;   The product is not larger than the largest integer the
;;;     language permits.
;;; Postconditions:
;;;   product is the product of the positive integers less than or
;;;   equal to n.  That is,
;;;     product = 1 * 2 * ... * n
(define factorial
  (lambda (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))

;;; Procedure:
;;;   count-from
;;; Parameters:
;;;   lower, an integer
;;;   upper, an integer
;;; Purpose:
;;;   Construct a list of the integers from lower to upper,
;;;   inclusive, in ascending order.
;;; Produces:
;;;   ls, a list
;;; Preconditions:
;;;   lower <= upper
;;;   Both lower and upper are numbers, exact, and integers.
;;; Postconditions:
;;;   The length of ls is 1 + upper - lower.
;;;   Every integer between lower and upper, inclusive, appears
;;;     in the list.
;;;   Every value in the list with a successor is smaller than its
;;;     successor.
;;;   For every integer k less than or equal to the length of
;;;       ls, the element in position k of ls is lower + k.
(define count-from
  (lambda (lower upper)
    (if (= lower upper)
        (list upper)
        (cons lower (count-from (+ lower 1) upper)))))

;;; Procedure:
;;;   termial
;;; Parameters:
;;;   number, a natural number
;;; Purpose:
;;;   Compute the sum of natural numbers not greater than a given
;;;   natural number
;;; Produces:
;;;   sum, a natural number
;;; Preconditions:
;;;   number is a number, exact, an integer, and non-negative.
;;;   The sum is not larger than the largest integer the language
;;;     permits.
;;; Postconditions:
;;;   sum is the sum of natural numbers not greater than number.
;;;   That is, sum = 0 + 1 + 2 + ... + number
(define termial
  (lambda (number)
    (cond
      ((not (number? number))
       (error "termial expects a number, received" number))
      ((not (integer? number))
       (error "termial expects an integer, received" number))
      ((< number 1)
       (error "termial expects a positive integer, received" number))
      ((inexact? number)
       (error "termial expects an exact integer, received" number))
      (else
       (termial-kernel number)))))
(define termial-kernel
  (lambda (number)
    (if (zero? number)
        0
        (+ number (termial-kernel (- number 1))))))

(define termial-local
  (letrec ([kernel (lambda (sum n)
                     (if (zero? n)
                         sum
                         (kernel (+ sum n) (- n 1))))])
    (lambda (n)
      (kernel 0 n))))

(define countdown
  (lambda (upper lower)
    (if (= upper lower)
        (list lower)
        (cons upper (countdown (- upper 1) lower)))))

(define countdown-0
  (lambda (upper)
    (if (zero? upper)
        (list upper)
        (cons upper (countdown-0 (- upper 1))))))

(define value-replicate
  (lambda (count value)
    (if (= 1 count)
        (list value)
        (cons value (value-replicate (- count 1) value)))))

(define my-iota
  (letrec ([reverse-iota (lambda (upper)
                           (if (= 0 upper)
                               (list upper)
                               (cons upper (reverse-iota (- upper 1)))))])
    (lambda (upper)
      (reverse (reverse-iota (- upper 1))))))

(define count-from-iota
  (lambda (n)
    (append 
     (reverse 
      (map -
          (map * (make-list n -1) (iota n)) (make-list n 1))) 
     (iota (+ n 1)))))