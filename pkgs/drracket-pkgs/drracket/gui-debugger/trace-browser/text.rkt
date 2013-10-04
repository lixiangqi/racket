#lang racket/base
(require racket/class
         racket/gui/base
         data/interval-map
         unstable/arrow
         framework
         data/interval-map
         macro-debugger/syntax-browser/interfaces)

(provide text:hover<%>
         text:hover-drawings<%>

         text:hover-mixin
         text:hover-drawings-mixin
         text:tacking-mixin
         text:region-data-mixin
         text:clickregion-mixin
         browser-text%)


;; A Drawing is (make-drawing (??? -> void) (box boolean))
(define-struct drawing (draw tacked?))

;; Interfaces

(define text:region-data<%>
  (interface (text:basic<%>)
    get-region-mapping))

(define text:hover<%>
  (interface (text:basic<%>)
    update-hover-position))

(define text:hover-drawings<%>
  (interface (text:basic<%>)
    add-hover-drawing
    get-position-drawings))


;; Mixins

(define text:region-data-mixin
  (mixin (text:basic<%>) (text:region-data<%>)

    (define table (make-hasheq))

    (define/public (get-region-mapping key)
      (hash-ref! table key (lambda () (make-interval-map))))

    (define/augment (after-delete start len)
      (for ([im (in-hash-values table)])
        (interval-map-contract! im start (+ start len)))
      (inner (void) after-delete start len))

    (define/augment (after-insert start len)
      (for ([im (in-hash-values table)])
        (interval-map-expand! im start (+ start len)))
      (inner (void) after-insert start len))

    (super-new)))

(define text:hover-mixin
  (mixin (text:basic<%>) (text:hover<%>)
    (inherit dc-location-to-editor-location
             find-position)

    (define/override (on-default-event ev)
      (super on-default-event ev)
      (case (send ev get-event-type)
        ((enter motion leave)
         (define-values (x y)
           (let ([gx (send ev get-x)]
                 [gy (send ev get-y)])
             (dc-location-to-editor-location gx gy)))
         (define on-it? (box #f))
         (define pos (find-position x y #f on-it?))
         (update-hover-position (and (unbox on-it?) pos)))))

    (define/public (update-hover-position pos)
      (void))

    (super-new)))

(define text:hover-drawings-mixin
  (mixin (text:hover<%> text:region-data<%>) (text:hover-drawings<%>)
    (inherit dc-location-to-editor-location
             find-position
             invalidate-bitmap-cache
             get-region-mapping)
    (super-new)

    ;; interval-map of Drawings
    (define drawings-list (get-region-mapping 'hover-drawings))

    (field [hover-position #f])

    (define/override (update-hover-position pos)
      (define old-pos hover-position)
      (super update-hover-position pos)
      (set! hover-position pos)
      (unless (same-drawings? old-pos pos)
        (invalidate-bitmap-cache 0.0 0.0 +inf.0 +inf.0)))

    (define/public (add-hover-drawing start end draw [tack-box (box #f)])
      (let ([drawing (make-drawing draw tack-box)])
        (interval-map-cons*! drawings-list
                             start (add1 end)
                             drawing
                             null)))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for ([d (get-position-drawings hover-position)])
          ((drawing-draw d) this dc left top right bottom dx dy))))

    (define/public (get-position-drawings pos)
      (if pos (interval-map-ref drawings-list pos null) null))

    (define/private (same-drawings? old-pos pos)
      ;; relies on order drawings added & list-of-eq?-struct equality
      (equal? (get-position-drawings old-pos)
              (get-position-drawings pos)))))

(define text:tacking-mixin
  (mixin (text:basic<%> text:hover-drawings<%>) ()
    (inherit get-canvas
             get-keymap
             get-position-drawings)
    (inherit-field hover-position)
    (super-new)

    (define tacked-table (make-hasheq))

    (define/override (on-local-event ev)
      (case (send ev get-event-type)
        ((right-down)
         (if (pair? (get-position-drawings hover-position))
             (send (get-canvas) popup-menu
                   (make-tack/untack-menu (get-position-drawings hover-position))
                   (send ev get-x)
                   (send ev get-y))
             (super on-local-event ev)))
        (else
         (super on-local-event ev))))

    ;; Clear tacked-table on any modification.
    ;; FIXME: possible to be more precise? (but not needed for macro stepper)
    (define/augment (after-delete start len)
      (set! tacked-table (make-hasheq))
      (inner (void) after-delete start len))
    (define/augment (after-insert start len)
      (set! tacked-table (make-hasheq))
      (inner (void) after-insert start len))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for ([draw (in-hash-keys tacked-table)])
          (draw this dc left top right bottom dx dy))))

    (define/private (make-tack/untack-menu drawings)
      (define menu (new popup-menu%))
      (define keymap (get-keymap))
      (define tack-item
        (new menu-item% (label "Tack")
             (parent menu)
             (callback (lambda _ (tack drawings)))))
      (define untack-item
        (new menu-item% (label "Untack")
             (parent menu)
             (callback (lambda _ (untack drawings)))))
      (send tack-item enable
            (for/or ([d (in-list drawings)]) (not (unbox (drawing-tacked? d)))))
      (send untack-item enable
            (for/or ([d (in-list drawings)]) (unbox (drawing-tacked? d))))
      (when (is-a? keymap keymap/popup<%>)
        (new separator-menu-item% (parent menu))
        (send keymap add-context-menu-items menu))
      menu)

    (define/private (tack drawings)
      (for ([d (in-list drawings)])
        (hash-set! tacked-table (drawing-draw d) #t)
        (set-box! (drawing-tacked? d) #t)))
    (define/private (untack drawings)
      (for ([d (in-list drawings)])
        (hash-remove! tacked-table (drawing-draw d))
        (set-box! (drawing-tacked? d) #f)))))

   

#|
text:clickregion-mixin

Like clickbacks, but:
  - use interval-map to avoid linear search
    (major problem w/ macro stepper and large expansions!)
  - callback takes position of click, not (start, end)
  - different rules for removal
  - TODO: extend to double-click
|#
(define text:clickregion-mixin
  (mixin (text:region-data<%>) ()
    (inherit get-admin
             get-region-mapping
             dc-location-to-editor-location
             find-position)

    (super-new)

    ;; Two mappings: one for left clicks, another for right
    ;; mouse-downs.  Rationale: macro stepper wants to handle left
    ;; clicks normally, but wants to insert behavior (ie, change
    ;; focus) before normal processing of right-down (ie, editor
    ;; passes to keymap, opens popup menu).
    (define clickbacks (get-region-mapping 'click-region))
    (define right-clickbacks (get-region-mapping 'right-click-region))
    (define tracking #f)

    (define/public (set-clickregion start end callback [region 'click])
      (let ([mapping
             (case region
               ((click) clickbacks)
               ((right-down) right-clickbacks)
               (else (error 'set-clickregion
                            "bad region symbol: expected 'click or 'right-down, got ~e"
                            region)))])
        (if callback
            (interval-map-set! mapping start end callback)
            (interval-map-remove! mapping start end))))

    (define/private (get-event-position ev)
      (define-values (x y)
        (let ([gx (send ev get-x)]
              [gy (send ev get-y)])
          (dc-location-to-editor-location gx gy)))
      (define on-it? (box #f))
      (define pos (find-position x y #f on-it?))
      (and (unbox on-it?) pos))

    ;; on-default-event called if keymap does not handle event
    (define/override (on-default-event ev)
      (define admin (get-admin))
      (when admin
        (define pos (get-event-position ev))
        (case (send ev get-event-type)
          ((left-down)
           (set! tracking (and pos (interval-map-ref clickbacks pos #f)))
           (send admin update-cursor))
          ((left-up)
           (when tracking
             (let ([cb (and pos (interval-map-ref clickbacks pos #f))]
                   [tracking* tracking])
               (set! tracking #f)
               (when (eq? tracking* cb)
                 (cb pos)))
             (send admin update-cursor)))))
      (super on-default-event ev))

    ;; on-local-event called before keymap consulted
    (define/override (on-local-event ev)
      (case (send ev get-event-type)
        ((right-down)
         (when (get-admin)
           (define pos (get-event-position ev))
           (let ([cb (and pos (interval-map-ref right-clickbacks pos #f))])
             (when cb (cb pos))))))
      (super on-local-event ev))))

(define browser-text%
  (let ([browser-text-default-style-name "widget.rkt::browser-text% basic"])
    (class (text:clickregion-mixin
             (text:tacking-mixin
              (text:hover-drawings-mixin
               (text:hover-mixin
                (text:region-data-mixin
                 (text:hide-caret/selection-mixin
                  (text:foreground-color-mixin
                   (editor:standard-style-list-mixin text:basic%))))))))
      (inherit set-autowrap-bitmap get-style-list)
      (define/override (default-style-name) browser-text-default-style-name)
      (super-new (auto-wrap #t))
      (let* ([sl (get-style-list)]
             [standard (send sl find-named-style (editor:get-default-color-style-name))]
             [browser-basic (send sl find-or-create-style standard
                                  (make-object style-delta% 'change-family 'default))])
        (send sl new-named-style browser-text-default-style-name browser-basic))
      (set-autowrap-bitmap #f))))
