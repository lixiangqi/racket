#lang racket/base

(require racket/class
         racket/gui/base
         racket/path
         framework
         "browser.rkt")

(provide make-trace-browser
         trace-struct)

(define (make-trace-browser traces fn)
  (define frame (new trace-frame%
                     [filename fn]))
  (send (send frame get-widget) update-traces traces)
  (send frame show #t)
  frame)

(define trace-frame%
  (class (frame:basic-mixin frame%)
    (init-field (filename #f))
    
    (inherit get-area-container
             set-label)
    
    (super-new (label (make-label))
               (width 800)
               (height 600))
    
    (define obsoleted? #f)
    
    (define warning-panel
      (new horizontal-panel%
           (parent (get-area-container))
           (stretchable-height #f)
           (style '(deleted))))
    
    (define widget (new widget% 
                        [parent (get-area-container)]))
    
    (define/public (get-widget) widget)
         
    (define/private (make-label)
      (if filename
          (string-append (path->string
                          (file-name-from-path filename))
                         (if obsoleted? " (old)" "")
                         " - Trace browser")
          "Trace browser"))
    
    (define/public (add-obsoleted-message)
      (unless obsoleted?
        (set! obsoleted? #t)
        (new warning-canvas%
             (warning
              (string-append
               "Warning: This trace browser session is obsolete. "
               "The program may have changed."))
             (parent warning-panel))
        (set-label (make-label))
        (send (get-area-container) change-children
              (lambda (children)
                (cons warning-panel
                      (remq warning-panel children))))))))


(define warning-color "yellow")
(define warning-font normal-control-font)

(define warning-canvas%
  (class canvas%
    (init-field warning)
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (send dc set-font warning-font) 
        (let-values ([(cw ch) (get-client-size)]
                     [(tw th dont-care dont-care2) 
                      (send dc get-text-extent warning)])
          (send dc set-pen 
                (send the-pen-list find-or-create-pen warning-color 1 'solid))
          (send dc set-brush
                (send the-brush-list find-or-create-brush warning-color 'solid))
          (send dc draw-rectangle 0 0 cw ch)
          (send dc draw-text 
                warning
                (- (/ cw 2) (/ tw 2))
                (- (/ ch 2) (/ th 2))))))
    (super-new)
    (inherit min-width min-height stretchable-height)
    (let-values ([(tw th dc dc2)
                  (send (get-dc) get-text-extent warning warning-font)])
      (min-width (+ 2 (inexact->exact (ceiling tw))))
      (min-height (+ 2 (inexact->exact (ceiling th)))))
    (stretchable-height #f)))