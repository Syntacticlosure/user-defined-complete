#lang racket
(provide (all-defined-out))

(define (results imods user-def)
  (cons imods user-def))

(define (results-imported-modules res)
  (car res))
(define (results-user-defined-identifiers res)
  (cdr res))