#lang racket/base
(require racket/class
         racket/gui/base
         data/interval-map
         framework
         data/interval-map)

(provide text:region-data-mixin
         text:clickregion-mixin
         browser-text%)

(define text:region-data-mixin
  (mixin (text:basic<%>) ()

    (define table (make-hasheq))
    
    (define/augment (after-delete start len)
      (for ([im (in-hash-values table)])
        (interval-map-contract! im start (+ start len)))
      (inner (void) after-delete start len))

    (define/augment (after-insert start len)
      (for ([im (in-hash-values table)])
        (interval-map-expand! im start (+ start len)))
      (inner (void) after-insert start len))

    (super-new)))

(define text:clickregion-mixin
  (mixin (text:basic<%>) ()
    (inherit get-admin
             dc-location-to-editor-location
             find-position)

    (super-new)

    (define clickbacks (make-interval-map))
    (define arrow-cursor (make-object cursor% 'arrow))
    (define tracking #f)

    (define/public (set-clickregion start end callback)
      (if callback
          (interval-map-set! clickbacks start end callback)
          (interval-map-remove! clickbacks start end)))

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
    
    (define/override (adjust-cursor ev)
      (define pos (get-event-position ev))
      (define cb (and pos (interval-map-ref clickbacks pos #f)))
      (if cb
          arrow-cursor
          (super adjust-cursor ev)))))

(define browser-text%
  (let ([browser-text-default-style-name "browser-text% basic"])
    (class (text:clickregion-mixin
             (text:region-data-mixin
               (text:hide-caret/selection-mixin
                 (text:foreground-color-mixin
                   (editor:standard-style-list-mixin text:basic%)))))
      (inherit set-autowrap-bitmap get-style-list)
      
      (field [var-table (make-hasheq)])
      
      (define/public (set-var-table t) (set! var-table t))
      
      (define/override (default-style-name) browser-text-default-style-name)
      
      (super-new (auto-wrap #t))
      (let* ([sl (get-style-list)]
             [standard (send sl find-named-style (editor:get-default-color-style-name))]
             [browser-basic (send sl find-or-create-style standard
                                  (make-object style-delta% 'change-family 'default))])
        (send sl new-named-style browser-text-default-style-name browser-basic))
      (set-autowrap-bitmap #f))))
