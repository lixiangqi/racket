#lang racket/base
(require racket/class
         racket/gui/base
         racket/promise
         data/interval-map
         framework
         unstable/class-iop
         "pretty-printer.rkt"
         "interface.rkt"
         "util.rkt")
(provide print-syntax-to-editor)

(define-syntax-rule (uninterruptible e ...)
  (parameterize-break #f (begin e ...)))

;; print-syntax-to-editor : syntax text controller<%> config number number
;;                       -> display<%>
;; Note: must call display<%>::refresh to finish styling.
(define (print-syntax-to-editor stx text controller config
                                [insertion-point (send text last-position)])
  (define output-port (open-output-string/count-lines))
  (define range (pretty-print-syntax stx 
                                     output-port 
                                     (make-immutable-hasheq null)
                                     #t))
  (define output-string (get-output-string output-port))
  (define output-length (sub1 (string-length output-string))) ;; skip final newline
  (with-unlock text
    (uninterruptible
     (send text insert output-length output-string insertion-point))
    (new display%
         (text text)
         (controller controller)
         (range range)
         (start-position insertion-point)
         (end-position (+ insertion-point output-length)))))

;; display%
;; Note: must call refresh method to finish styling.
(define display%
  (class* object% (display<%>)
    (init-field/i [controller controller<%>]
                  [range range<%>])
    (init-field text
                start-position
                end-position)

    (define base-style
      (send (send text get-style-list) find-named-style (editor:get-default-color-style-name)))

    ;; extra-styles : hash[stx => (listof style-delta)]
    ;; Styles to be re-applied on every refresh.
    (define extra-styles (make-hasheq))

    ;; to-undo-styles : (listof (cons nat nat))
    ;; Ranges to unbold or unhighlight when selection changes.
    ;; FIXME: ought to be managed by text:region-data (to auto-update ranges)
    ;;   until then, positions are relative
    (define to-undo-styles null)

    ;; initialize : -> void
    (define/private (initialize)
      (uninterruptible
       (send text change-style base-style start-position end-position #f))
      (uninterruptible (add-clickbacks)))

    ;; add-clickbacks : -> void
    (define/private (add-clickbacks)
      (define mapping (make-interval-map))
      (define lazy-interval-map-init
        (delay
          (uninterruptible
           (for ([range (send/i range range<%> all-ranges)])
             (let ([stx (range-obj range)]
                   [start (range-start range)]
                   [end (range-end range)])
               (interval-map-set! mapping (+ start-position start) (+ start-position end) stx))))))
      (define (the-callback position)
        (force lazy-interval-map-init)
        (send/i controller selection-manager<%> set-selected-syntax
                (interval-map-ref mapping position #f)))
      (send text set-clickregion start-position end-position the-callback))

    ;; refresh : -> void
    ;; Clears all highlighting and reapplies all non-foreground styles.
    (define/public (refresh)
      (with-unlock text
        (uninterruptible
         (let ([undo-select/highlight-d (get-undo-select/highlight-d)])
           (for ([r (in-list to-undo-styles)])
             (send text change-style undo-select/highlight-d
                   (relative->text-position (car r))
                   (relative->text-position (cdr r)))))
         (set! to-undo-styles null))
        (uninterruptible
         (apply-extra-styles))
        (let ([selected-syntax
               (send/i controller selection-manager<%>
                       get-selected-syntax)])
          (uninterruptible
           (apply-selection-styles selected-syntax)))))

    ;; get-range : -> range<%>
    (define/public (get-range) range)

    ;; get-start-position : -> number
    (define/public (get-start-position) start-position)

    ;; get-end-position : -> number
    (define/public (get-end-position) end-position)

    ;; highlight-syntaxes : (list-of syntax) string -> void
    (define/public (highlight-syntaxes stxs hi-color)
      (let ([delta (highlight-style-delta hi-color)])
        (for ([stx (in-list stxs)])
          (hash-set! extra-styles stx
                     (cons delta (hash-ref extra-styles stx null))))))
    
    ;; Secondary Styling
    ;; May change in response to user actions

    ;; apply-extra-styles : -> void
    ;; Applies externally-added styles (such as highlighting)
    (define/private (apply-extra-styles)
      (for ([(stx deltas) (in-hash extra-styles)])
        (for ([r (in-list (send/i range range<%> get-ranges stx))])
          (for ([delta (in-list deltas)])
            (restyle-range r delta #t)))))

    ;; apply-selection-styles : syntax -> void
    ;; Styles subterms eq to the selected syntax
    (define/private (apply-selection-styles selected-syntax)
      (for ([r (in-list (send/i range range<%> get-ranges selected-syntax))])
        (restyle-range r select-d #t)))

    ;; restyle-range : (cons num num) style-delta% boolean -> void
    (define/private (restyle-range r style need-undo?)
      (when need-undo? (set! to-undo-styles (cons r to-undo-styles)))
      (send text change-style style
            (relative->text-position (car r))
            (relative->text-position (cdr r))))

    ;; relative->text-position : number -> number
    (define/private (relative->text-position pos)
      (+ pos start-position))

    ;; Initialize
    (super-new)
    (send/i controller controller<%> add-syntax-display this)
    (initialize)))

(define (open-output-string/count-lines)
  (let ([os (open-output-string)])
    (port-count-lines! os)
    os))

;; Color translation

;; translate-color : color-string -> color%
(define (translate-color color-string)
  (make-object color% color-string))

;; lightness-invert : uint8 uint8 uint8 -> (values uint8 uint8 uint8)
(define (lightness-invert r g b)
  (define (c x)
    (/ (exact->inexact x) 255.0))
  (define (d x)
    (inexact->exact (round (* x 255))))
  (let-values ([(r g b) (lightness-invert* (c r) (c g) (c b))])
    (values (d r) (d g) (d b))))

(define (lightness-invert* R G B)
  (let-values ([(Hp Sl L) (rgb->hsl* R G B)])
    (hsl*->rgb Hp Sl (- 1.0 L))))

(define (rgb->hsl* R G B)
  (define M (max R G B))
  (define m (min R G B))
  (define C (- M m))
  (define Hp
    (cond [(zero? C)
           ;; Undefined, but use 0
           0.0]
          [(= M R)
           (realmod* (/ (- G B) C) 6)]
          [(= M G)
           (+ (/ (- B R) C) 2)]
          [(= M B)
           (+ (/ (- R G) C) 4)]))
  (define L (* 0.5 (+ M m)))
  (define Sl
    (cond [(zero? C) 0.0]
          [(>= L 0.5) (/ C (* 2 L))]
          [else (/ C (- 2 (* 2 L)))]))
  
  (values Hp Sl L))

(define (hsl*->rgb Hp Sl L)
  (define C
    (cond [(>= L 0.5) (* 2 L Sl)]
          [else (* (- 2 (* 2 L)) Sl)]))
  (define X (* C (- 1 (abs (- (realmod Hp 2) 1)))))
  (define-values (R1 G1 B1)
    (cond [(< Hp 1) (values C X 0)]
          [(< Hp 2) (values X C 0)]
          [(< Hp 3) (values 0 C X)]
          [(< Hp 4) (values 0 X C)]
          [(< Hp 5) (values X 0 C)]
          [(< Hp 6) (values C 0 X)]))
  (define m (- L (* 0.5 C)))
  (values (+ R1 m) (+ G1 m) (+ B1 m)))

;; realmod : real integer -> real
;; Adjusts a real number to [0, base]
(define (realmod x base)
  (define xint (ceiling x))
  (define m (modulo xint base))
  (realmod* (- m (- xint x)) base))

;; realmod* : real real -> real
;; Adjusts a number in [-base, base] to [0,base]
;; Not a real mod, but faintly reminiscent.
(define (realmod* x base)
  (if (negative? x)
      (+ x base)
      x))

;; Styles

(define select-d
  (make-object style-delta% 'change-weight 'bold))

(define (highlight-style-delta color)
  (let ([sd (new style-delta%)])
    (send sd set-delta-background color)
    sd))

(define (get-undo-select/highlight-d)
  (let ([sd (make-object style-delta% 'change-weight 'normal)]
        [bg "white"])
    (send sd set-delta-background bg)
    sd))
