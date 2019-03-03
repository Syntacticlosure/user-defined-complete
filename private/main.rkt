#lang racket
(require racket/set syntax/kerncase "results.rkt"
         syntax/parse)
(provide (rename-out [walk-wrapper walk]))

(define (walk-wrapper stx)
  (define imported-modules (seteq))

  (define (extract-module-path stx)
    (syntax-parse stx #:literals (quote)
      [(~or x:id (quote x:id)) (set! imported-modules (set-add imported-modules (syntax-e #'x)))]
      [x:string (void)]
      [_ (void)]
      ))

  (define (extract-phaseless stx)
    (syntax-parse stx #:literals (only prefix rename)
      #:datum-literals (all-except prefix-all-except)
      [(only modpath id ...) (void)]
      [(prefix prefix-id modpath) (extract-module-path #'modpath)]
      [(all-except modpath id ...) (extract-module-path #'modpath)]
      [(prefix-all-except prefix-id
                          modpath id ...) (extract-module-path #'modpath)]
      [(rename modpath local-id exported-id) (void)]
      [modpath (extract-module-path #'modpath)]))

  (define (extract-require-spec stx)
    (syntax-parse stx #:literals (for-meta for-syntax for-template for-label)
      #:datum-literals (just-meta)
      [((~or (~seq for-meta pl)
             for-syntax
             for-template
             for-label)
             phaseless ...) (for-each extract-phaseless
                                             (syntax->list #'(phaseless ...)))]
      [(just-meta pl spec ...)
       (for-each extract-require-spec (syntax->list #'(spec ...)))]
      [phaseless (extract-phaseless #'phaseless)]
      ))
      

  (define (walk* stxs)
    (let loop ([ls (syntax->list stxs)])
      (if (null? ls)
          (seteq)
          (set-union (walk (car ls)) (loop (cdr ls))))))

  (define (visible? id)
    (for/and ([scope (in-list
                      (hash-ref (syntax-debug-info id)
                                'context (λ () '())))])
      (not (eq? 'macro (vector-ref scope 1)))))

  (define (visible stx)
    (syntax-case stx ()
      [(a . b)
       (set-union (visible #'a) (visible #'b))]
      [x
       (identifier? #'x)
       (if (visible? #'x)
           (seteq (syntax-e #'x))
           (seteq))]
      [_ (seteq)]))

  (define (walk stx)
    (define ret
      (kernel-syntax-case stx #f
        [(#%expression ?expr) (walk #'?expr)]
        [(module _ imod (#%plain-module-begin ?module-level-form ...))
         (begin (extract-module-path #'imod)
                (walk* #'(?module-level-form ...)))]
        [(begin ?expr ...)
         (walk* #'(?expr ...))]
        [(begin0 ?expr ...)
         (walk* #'(?expr ...))]
        [(begin-for-syntax ?expr ...)
         (walk* #'(?expr ...))]
        [(define-values (?id ...) ?expr)
         (set-union (visible #'(?id ...)) (walk #'?expr))]
        [(define-syntaxes (?id ...) ?expr)
         (set-union (visible #'(?id ...)) (walk #'?expr))]
        [(#%plain-lambda ?formals ?expr ...)
         (set-union (visible #'?formals) (walk* #'(?expr ...)))]
        [(case-lambda (?formals ?expr ...) ...)
         (let loop ([formals (syntax->list #'(?formals ...))]
                    [set (walk* #'(?expr ... ...))])
           (cond
             [(null? formals) set]
             [else (loop (cdr formals)
                         (set-union (visible (car formals))
                                    set))]))]
        [(if ?expr ...)
         (walk* #'(?expr ...))]
        [(let-values ([(?id ...) ?expr] ...)
           ?body ...)
         (set-union (visible #'(?id ... ...))
                    (walk* #'(?expr ... ?body ...)))]
        [(letrec-values ([(?id ...) ?expr] ...)
           ?body ...)
         (set-union (visible #'(?id ... ...))
                    (walk* #'(?expr ... ?body ...)))]
        [(set! ?id ?expr)
         (walk #'?expr)]
        [(with-continuation-mark ?expr ...)
         (walk* #'(?expr ...))]
        [(#%plain-app ?expr ...)
         (walk* #'(?expr ...))]
        [(#%require spec ...)
         (begin (for-each extract-require-spec (syntax->list #'(spec ...)))
         (seteq))]
        [_ (seteq)]))
  
    (cond
      [(syntax-property stx 'disappeared-binding)
       =>
       (λ (ls)
         (set-union (visible (datum->syntax #'k ls)) ret))]
      [else ret]))
  (define first (walk stx))
  (results imported-modules first))