#lang racket/base

(require "main.rkt" racket/set "results.rkt")
(provide go)
(define (go v path src cust)
  (cond
    [(exn? v) #f]
    [else
     (define walk-results (walk v))
     (results (set->list (results-imported-modules walk-results))
      (for/list ([s (in-set (results-user-defined-identifiers walk-results))])
       (symbol->string s)))]))