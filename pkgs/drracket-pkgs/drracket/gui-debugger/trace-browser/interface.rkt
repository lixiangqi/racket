#lang racket/base
(require unstable/class-iop
         (for-syntax racket/base))
(provide (all-defined-out))

;; Helpers

(define-for-syntax (join . args)
  (define (->string x)
    (cond [(string? x) x]
          [(symbol? x) (symbol->string x)]
          [(identifier? x) (symbol->string (syntax-e x))]
          [else (error '->string)]))
  (string->symbol (apply string-append (map ->string args))))

;; not in notify.rkt because notify depends on gui
(define-interface-expander methods:notify
  (lambda (stx)
    (syntax-case stx ()
      [(_ name ...)
       (datum->syntax #f
         (apply append
                (for/list ([name (syntax->list #'(name ...))])
                  (list ;; (join "init-" #'name)
                   (join "get-" name)
                   (join "set-" name)
                   (join "listen-" name)))))])))

;; Interfaces
;; displays-manager<%>
(define-interface displays-manager<%> ()
  (;; add-syntax-display : display<%> -> void
   add-syntax-display

   ;; remove-all-syntax-displays : -> void
   remove-all-syntax-displays))

;; selection-manager<%>
(define-interface selection-manager<%> ()
  (;; selected-syntax : notify-box of syntax/#f
   (methods:notify selected-syntax)))


;; mark-manager<%>
;; Manages marks, mappings from marks to colors
(define-interface mark-manager<%> ()
  (;; get-primary-partition : -> partition
   get-primary-partition

   ;; reset-primary-partition : -> void
   reset-primary-partition))

;; controller<%>
(define-interface controller<%> (selection-manager<%>
                                 mark-manager<%>)
  ())

;; display<%>
(define-interface display<%> ()
  (;; refresh : -> void
   refresh

   ;; highlight-syntaxes : (list-of syntax) color -> void
   highlight-syntaxes
   underline-syntax
   ;; get-start-position : -> number
   get-start-position

   ;; get-end-position : -> number
   get-end-position

   ;; get-range : -> range<%>
   get-range))

;; range<%>
(define-interface range<%> ()
  (;; get-ranges : datum -> (list-of (cons number number))
   get-ranges
   get-range-by-pos
   ;; get-treeranges : -> (listof TreeRange)
   get-treeranges

   ;; all-ranges : (list-of Range)
   ;; Sorted outermost-first
   all-ranges

   ;; get-identifier-list : (list-of identifier)
   get-identifier-list))


;; A Range is (make-range datum number number)
(define-struct range (obj start end))

;; A TreeRange is (make-treerange syntax nat nat (listof TreeRange))
;; where subs are disjoint, in order, and all contained within [start, end]
(define-struct treerange (obj start end subs))

(define-interface partition<%> ()
  (;; get-partition : any -> number
   get-partition

   ;; same-partition? : any any -> number
   same-partition?

   ;; count : -> number
   count))
