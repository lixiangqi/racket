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

(define widget%
  (class object%
    (init parent)

    (field [controller (new controller%)]
           [traces empty]
           [step 0])
    
    (define log-text
      (new (class text%
             
             (inherit begin-edit-sequence
                      end-edit-sequence
                      lock
                      last-position
                      insert
                      delete
                      find-line
                      change-style
                      line-paragraph
                      dc-location-to-editor-location
                      paragraph-start-position
                      paragraph-end-position
                      )
             (super-new)
             
             (define var-logs empty)
             (define mark-num 0)
             (define bold-sd (make-object style-delta% 'change-weight 'bold))
             (define normal-sd (make-object style-delta% 'change-weight 'normal))
             
             (define/public (display-logs logs mark?)
               (begin-edit-sequence)
               (lock #f)
               (set! var-logs logs)
               (delete 0 (last-position))
               (for-each (lambda (l) (insert l)) logs)
               (when mark?
                 (change-style normal-sd 0 (last-position))
                 (change-style bold-sd 
                               (paragraph-start-position mark-num)
                               (paragraph-end-position mark-num)))
               (lock #t)
               (end-edit-sequence))
             
             (define/private (move-to-view num)
               (set! mark-num num)
               (display-logs var-logs #t))
             
             (define/override (on-event evt)
               (let*-values ([(x y) (dc-location-to-editor-location (send evt get-x) (send evt get-y))]
                             [(line) (find-line y)]
                             [(paragraph) (line-paragraph line)])
                 (case (send evt get-event-type)
                   [(left-down)
                    (when (< paragraph (length var-logs))
                      (move-to-view paragraph)
                      (update-view-text paragraph))])))
                             
               
               
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
    
    (define slider-panel 'uninitialized-slider-panel)
    (define navigator 'uninitialized-navigator)
    (define previous-button 'uninitialized-previous-button)
    (define next-button 'uninitialized-next-button)
    (define status-msg 'uninitialized-status-msg)
    
    (define/private (initialize-navigator)
      (let ([navigate-previous-icon (compiled-bitmap (step-back-icon #:color run-icon-color #:height (toolbar-icon-height)))]
            [navigate-next-icon (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height)))])
        (set! slider-panel (new horizontal-panel% [parent view-panel] [stretchable-width #f] [stretchable-height #f]))
        (new slider% [label #f] [min-value 1] [max-value 200] [parent slider-panel] [style (list 'horizontal 'plain)])      
        (set! navigator (new horizontal-panel% [parent view-panel] [stretchable-height #f] [alignment '(center center)]))
        (set! previous-button (new button% [label (list navigate-previous-icon "Step" 'left)] [parent navigator]))
        (set! next-button (new button% 
                               [label (list navigate-next-icon "Step" 'right)]
                               [parent navigator]
                               [callback (lambda (b e) (navigate-next))]))
        (set! status-msg (new message% [label "set"] [parent navigator] [stretchable-width #t]))))
    
    (define/private (navigate-next)
      (void))
      
    
    (define/public (set-traces trace) 
      (set! traces (map (lambda (t) (trace-struct (first t) (second t) (third t) (fourth t))) trace)))
    
    (define/public (display-traces)
      (let ([logs (map (lambda (t) (format "~a: ~v\n" (syntax->datum (trace-struct-id-stx t)) (trace-struct-value t))) traces)])
        (send log-text display-logs logs #f)))
      
             
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
    
    (define/public (initialize-view-text)
      (let ([sd (new style-delta%)])
        (with-unlock view-text
          (send sd set-delta-foreground "gray")
          (send view-text insert "No trace selected\n")
          (send view-text change-style sd (send view-text paragraph-start-position 0) 
                                          (send view-text paragraph-end-position 0)))))
    
    (define/private (update-view-text n)
      (let* ([steps (map first (continuation-mark-set-first (trace-struct-ccm (list-ref traces n)) 'inspect null))])
        (printf "steps = ~a\n" (length steps))
        (with-unlock view-text
        (send view-text erase))
        (add-syntax (first steps))
        (add-separator)
        (add-syntax (second steps))
        (initialize-navigator)
        (send status-msg set-label "update")))

    ;; Initialize
    (super-new)
    (initialize-view-text)
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 2/3))
    (send split-panel end-container-sequence)))