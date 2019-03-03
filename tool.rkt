#lang racket
(require drracket/tool racket/gui framework racket/runtime-path
         racket/fasl syntax/location "private/results.rkt")
(provide tool@)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module backend racket/base
  (require racket/place racket/port racket/fasl scribble/xref scribble/manual-struct
           racket/set)
  (provide run)
  
  (define (run pch)
    (place-channel-put pch (load)))

  (define (load)
    (define data
      (call-with-output-bytes
       (λ (p)
         (s-exp->fasl (get-completions/manuals) p))))
    (define shared (make-shared-bytes (bytes-length data)))
    (bytes-copy! shared 0 data)
    shared)
  
  (define (get-completions/manuals)
    (define xref
      (let ([load-collections-xref
             ;; Make the dependency on `setup/xref' indirect, so that a
             ;; GUI does not depend on having documentation installed:
             (with-handlers ([exn:missing-module? (lambda (exn)
                                                    (lambda ()
                                                      (load-xref null)))])
               (dynamic-require 'setup/xref 'load-collections-xref))])
        (load-collections-xref)))
    
    (let ([ht (make-hasheq)])
      (for-each
       (λ (entry)
         (let ([desc (entry-desc entry)])
           (when (exported-index-desc? desc)
             (let ([name (exported-index-desc-name desc)])
               (when name
                 (for ([lib (exported-index-desc-from-libs desc)])
                   (hash-set! ht lib (cons (symbol->string name) (hash-ref ht lib '()))))
                 )))))
       (xref-index xref))
      ht)))

(define pch #f)
(define symbols #f)

(define (load-symbols)
  (cond
    [symbols symbols]
    [pch (set! symbols 
               (for/hasheq ([(k v) (in-hash
                                  (call-with-input-bytes
                                   (place-channel-get pch)
                                   fasl->s-exp))])
                 (values k (list->set v))
                 ))
                 (set! pch #f)
                 symbols]
[else (start-backend)
      (load-symbols)]))

(define (start-backend)
  (unless pch
    (set! pch (dynamic-place (quote-module-path backend) 'run))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path expansion.rkt "private/expansion.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define-local-member-name set-user-defined-identifiers)
    (define-local-member-name set-imported-modules)
    
    (define udc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (super-new)

        (define user-defined '())
        (define imported-mods (seteq))
        (define imported-words '())

        (define/public (set-user-defined-identifiers ls)
          (set! user-defined ls))

        (define/public (set-imported-modules imods)
          (unless (equal? imported-mods imods)
            (set! imported-mods imods)
            (set! imported-words
                  (set->list (apply set-union
                                    (cons (set)
                                          (for/list ([i (in-set imods)])
                                            (hash-ref (load-symbols) i (set))))))))
          )

        (define/override (get-all-words)
          (append user-defined imported-words))

        (thread
         (λ ()
           (sync (system-idle-evt))
           (queue-callback start-backend)))
        ))

    
    (drracket:module-language-tools:add-online-expansion-handler
     expansion.rkt 'go
     (λ (t v)
       (when v
         (send t set-user-defined-identifiers (results-user-defined-identifiers v))
         (send (send (send t get-tab) get-ints)
               set-user-defined-identifiers (results-user-defined-identifiers v))
         (define import-set  (list->seteq (results-imported-modules v)))
         (send t set-imported-modules import-set)
         (send (send (send t get-tab) get-ints)
               set-imported-modules import-set))))

    (define (phase1)
      (drracket:get/extend:extend-interactions-text udc-mixin #f)
      (drracket:get/extend:extend-definitions-text udc-mixin #f))
    ))
