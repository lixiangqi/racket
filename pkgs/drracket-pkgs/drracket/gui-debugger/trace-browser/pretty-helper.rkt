#lang racket/base
(require racket/pretty
         unstable/struct)

(provide (all-defined-out))

(define (pretty-print/defaults datum [port (current-output-port)])
  (parameterize
    (;; Printing parameters (defaults from MzScheme and DrScheme 4.2.2.2)
     [print-unreadable #t]
     [print-graph #f]
     [print-struct #t]
     [print-box #t]
     [print-vector-length #f]
     [print-hash-table #t])
    (pretty-write datum port)))

;; Rather than map stx to (syntax-e stx) where (syntax-e stx1) and (syntax-e stx2)
;; may be indistinguishable, map it to a different, unique value. Use syntax-dummy,
;; and extend pretty-print-remap-stylable to look inside.

(define-struct syntax-dummy (val))
(define-struct (id-syntax-dummy syntax-dummy) (remap))

;; syntax->datum/tables : syntax -> (values s-expr hashtable hashtable)
;;
;; Returns three values:
;;   - an S-expression
;;   - a hashtable mapping S-expressions to syntax objects
;;   - a hashtable mapping syntax objects to S-expressions
;; Syntax objects which are eq? will map to same flat values

(define (syntax->datum/tables stx)
  (let/ec escape
    (let ([flat=>stx (make-hasheq)]
          [stx=>flat (make-hasheq)])
      (define (loop obj)
        (cond [(hash-ref stx=>flat obj (lambda _ #f))
               => (lambda (datum) datum)]
              [(syntax? obj)
               (let ([lp-datum (loop (syntax-e obj))])
                 (hash-set! flat=>stx lp-datum obj)
                 (hash-set! stx=>flat obj lp-datum)
                 lp-datum)]
              ;; Traversable structures
              [(pair? obj)
               (pairloop obj)]
              [(prefab-struct-key obj)
               => (lambda (pkey)
                    (let-values ([(refold fields) (unfold-pstruct obj)])
                      (refold (map loop fields))))]
              [(vector? obj) 
               (list->vector (map loop (vector->list obj)))]
              [(box? obj)
               (box (loop (unbox obj)))]
              [(hash? obj)
               (let ([constructor
                      (cond [(hash-equal? obj) make-immutable-hash]
                            [(hash-eqv? obj) make-immutable-hasheqv]
                            [(hash-eq? obj) make-immutable-hasheq])])
                 (constructor
                  (for/list ([(k v) (in-hash obj)])
                    (cons k (loop v)))))]
              ;; Atoms ("confusable")
              [(symbol? obj)
               (make-id-syntax-dummy obj obj)]
              ;; null, boolean, number, keyword, string, bytes, char, regexp, 3D vals
              [else 
               (make-syntax-dummy obj)]))
      (define (pairloop obj)
        (cond [(pair? obj)
               (cons (loop (car obj))
                     (pairloop (cdr obj)))]
              [(null? obj)
               null]
              [(and (syntax? obj) (null? (syntax-e obj)))
               null]
              [else (loop obj)]))
      (values (loop stx)
              flat=>stx
              stx=>flat))))

;; unfold-pstruct : prefab-struct -> (values (list -> prefab-struct) list)
(define (unfold-pstruct obj)
  (define key (prefab-struct-key obj))
  (define fields (struct->list obj))
  (values (lambda (new-fields)
            (apply make-prefab-struct key new-fields))
          fields))

(define special-expression-keywords
  '(quote quasiquote unquote unquote-splicing syntax
    quasisyntax unsyntax unsyntax-splicing))