#lang racket/base
(require racket/class
         racket/gui/base
         framework
         unstable/class-iop
         "interface.rkt"
         "controller.rkt"
         "syntax-display.rkt"
         "hrule-snip.rkt"
         "text.rkt"
         "util.rkt" 
         images/compile-time
         (for-syntax racket/base
                     images/icons/control
                     images/icons/style)
         (except-in racket/list range))
(provide make-trace-browser)

(define (make-trace-browser traces)
  (define frame (new frame%
                     [label "Trace Browser"]
                     [width 800]
                     [height 600]))
  (define widget (new widget% [parent frame]))
  (send widget set-traces traces)
  (send widget display-traces)
  (send frame show #t))

;; widget%
;; A syntax widget creates its own syntax-controller.
(define widget%
  (class object%
    (init parent)

    (field [controller (new controller%)]
           [traces empty])
    
    (define log-text
      (new (class text%
             
             (inherit begin-edit-sequence
                      end-edit-sequence
                      lock
                      last-position
                      insert
                      delete
                      )
             (super-new)
             
             (define/public (display-logs logs)
               (begin-edit-sequence)
               (lock #f)
               (delete 0 (last-position))
               (for-each (lambda (l) (insert l)) logs)
               (lock #t)
               (end-edit-sequence))
               
               
             )))
    
    (struct trace-struct (id-stx value number ccm))
    (define main-panel
      (new vertical-panel% (parent parent)))
    (define split-panel
      (new panel:horizontal-dragable% (parent main-panel)))
    (define view-text (new browser-text%))
    (new editor-canvas% [parent split-panel] [editor log-text] [style '(auto-hscroll)])
    (define view-panel (new vertical-panel% [parent split-panel]))
    (define view-canvas (new canvas:color% (parent view-panel) (editor view-text)))
    
    (define slider-panel (new horizontal-panel% [parent view-panel] [stretchable-width #f] [stretchable-height #f]))
    (new slider% [label #f] [min-value 1] [max-value 200] [parent slider-panel] [style (list 'horizontal 'plain)])
    
    (define navigator (new horizontal-panel% [parent view-panel] [stretchable-width #f] [stretchable-height #f]))
    (define navigate-previous-icon
      (compiled-bitmap (step-back-icon #:color run-icon-color #:height (toolbar-icon-height))))
    (define navigate-next-icon
      (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height))))
    (define previous-button
      (new button% [label (list navigate-previous-icon "Step" 'left)] [parent navigator]))
    (define msg
      (new message% [label "Step 1 of 20"] [parent navigator]))
    (define next-button
      (new button% [label (list navigate-next-icon "Step" 'right)] [parent navigator]))
    
    (define/public (set-traces trace) 
      (set! traces (map (lambda (t) (trace-struct (first t) (second t) (third t) (fourth t))) trace)))
    
    (define/public (display-traces)
      (let ([logs (map (lambda (t) (format "~a: ~v\n" (syntax->datum (trace-struct-id-stx t)) (trace-struct-value t))) traces)])
        (send log-text display-logs logs)))
      
             
    (send view-text set-styles-sticky #f)
    (send view-text lock #t)
    
    (define/public (add-text text)
      (printf "add-text:text=~a\n" text)
      (with-unlock view-text
        (send view-text insert text)))
    
    (define/public (add-syntax stx
                               #:binders [binders '#hash()]
                               #:shift-table [shift-table '#hash()]
                               #:definites [definites #f]
                               #:hi-colors [hi-colors null]
                               #:hi-stxss [hi-stxss null]
                               #:substitutions [substitutions null])
      (define (get-shifted id) (hash-ref shift-table id null))
      
      (with-unlock view-text
        (define display
          (print-syntax-to-editor stx view-text controller
                                  (calculate-columns)
                                  (send view-text last-position)))
        (send view-text insert "\n")
        (define range (send/i display display<%> get-range))
        (define offset (send/i display display<%> get-start-position))
        
         ;; Apply highlighting
;        (set! hi-stxss (list stx))
;        (send/i display display<%> highlight-syntaxes hi-stxss "MistyRose")
        
        #;(with-log-time "highlights"
         (for ([hi-stxs (in-list hi-stxss)] [hi-color (in-list hi-colors)])
           (send/i display display<%> highlight-syntaxes hi-stxs hi-color)))       
        
        (send display refresh)))

    (define/public (add-separator)
      (with-unlock view-text
        (send* view-text
          (insert (new hrule-snip%))
          (insert "\n"))))

    (define/public (erase-all)
      (with-unlock view-text
        (send view-text erase))
      (send/i controller displays-manager<%> remove-all-syntax-displays))

    (define/public (get-text) view-text)

    (define/private (calculate-columns)
      (define style (send (send view-text get-style-list) find-named-style (editor:get-default-color-style-name)))
      (define char-width (send style get-text-width (send view-canvas get-dc)))
      (let ([admin (send view-text get-admin)]
            [w-box (box 0.0)])
        (send admin get-view #f #f w-box #f)
        (sub1 (inexact->exact (floor (/ (unbox w-box) char-width))))))

    ;; Initialize
    (super-new)
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 2/3))
    (send split-panel end-container-sequence)))