#lang racket/base
(require racket/class
         unstable/class-iop
         unstable/gui/notify
         ffi/unsafe
         "interface.rkt")
(provide controller%)

(define get-marks
  (get-ffi-obj "scheme_stx_extract_marks" (ffi-lib #f)
               (_fun _scheme -> _scheme)))

(define bound-partition%
  (class* object% (partition<%>)

    ;; simplified : hash[(listof nat) => nat]
    (define simplified (make-hash))

    ;; next-number : nat
    (define next-number 0)

    (define/public (get-partition stx)
      (let ([marks (get-marks stx)])
        (or (hash-ref simplified marks #f)
            (let ([n next-number])
              (hash-set! simplified marks n)
              (set! next-number (add1 n))
              n))))

    (define/public (same-partition? a b)
      (= (get-partition a) (get-partition b)))

    (define/public (count)
      next-number)

    (get-partition (datum->syntax #f 'nowhere))
    (super-new)))

;; displays-manager-mixin
(define displays-manager-mixin
  (mixin () (displays-manager<%>)
    ;; displays : (list-of display<%>)
    (field [displays null])

    ;; add-syntax-display : display<%> -> void
    (define/public (add-syntax-display c)
      (set! displays (cons c displays)))

    ;; remove-all-syntax-displays : -> void
    (define/public (remove-all-syntax-displays)
      (set! displays null))

    (super-new)))

;; selection-manager-mixin
(define selection-manager-mixin
  (mixin (displays-manager<%>) (selection-manager<%>)
    (inherit-field displays)
    (define-notify selected-syntax (new notify-box% (value #f)))

    (super-new)
    (listen-selected-syntax
     (lambda (new-value)
       (for-each (lambda (display) 
                   (send/i display display<%> refresh))
                 displays)))))

;; mark-manager-mixin
(define mark-manager-mixin
  (mixin () (mark-manager<%>)
    (init-field/i [primary-partition partition<%> (new bound-partition%)])
    (super-new)

    ;; get-primary-partition : -> partition
    (define/public-final (get-primary-partition)
      primary-partition)

    ;; reset-primary-partition : -> void
    (define/public-final (reset-primary-partition)
      (set! primary-partition (new bound-partition%)))))

(define controller%
  (class* (selection-manager-mixin
           (mark-manager-mixin
            (displays-manager-mixin
              object%)))
    (controller<%>)
    (super-new)))
