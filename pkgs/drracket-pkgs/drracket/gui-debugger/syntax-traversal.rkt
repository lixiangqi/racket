#lang racket

(provide get-pos-table
         traverse-stx)

(define pos-table (make-hasheq))
(define (get-pos-table) pos-table)

(define (traverse-stx s)
  (cond
    [(symbol? s) (void)]
    [(empty? s) (void)]
    [(syntax? s)
     (hash-set! pos-table (syntax-position s) s)
     (traverse-stx (syntax-e s))]
    [(pair? s)
     (traverse-stx (car s))
     (traverse-stx (cdr s))]
    [(vector? s)
     (vector-map traverse-stx s)]
    [(box? s)
     (traverse-stx (unbox s))]
    [(hash? s) (for-each traverse-stx (hash-values s))]
    [(struct? s) (traverse-stx s)]
    [else (void)]))
    
     
    
     
     
     
     
  