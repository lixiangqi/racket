#lang racket/base
(require racket/class
         racket/gui/base
         framework
         unstable/class-iop
         unstable/gui/notify
         "interface.rkt"
         "controller.rkt"
         "syntax-display.rkt"
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
           [current-marks empty]
           [function-calls empty]
           [last-app-list empty]
           [step 1]
           [limit 0])
    
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
    
    (define navigator 'uninitialized-navigator)
    (define previous-button 'uninitialized-previous-button)
    (define next-button 'uninitialized-next-button)
    (define status-msg 'uninitialized-status-msg)
    (define slider 'uninitialized-slider)
    
    (define/private (initialize-navigator)
      (let ([navigate-previous-icon (compiled-bitmap (step-back-icon #:color run-icon-color #:height (toolbar-icon-height)))]
            [navigate-next-icon (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height)))]
            [slider-panel (new horizontal-panel% [parent view-panel] [stretchable-width #f] [stretchable-height #f])])
        (set! slider (new slider% 
                          [label #f] 
                          [min-value 1] 
                          [max-value limit] 
                          [parent slider-panel] 
                          [style (list 'horizontal 'plain)]
                          [callback (lambda (b e) (set-current-step (send slider get-value)))]))
        (set! navigator (new horizontal-panel% [parent view-panel] [stretchable-height #f] [alignment '(center center)]))
        (new message% [label ""] [parent navigator] [stretchable-width #t])
        (set! previous-button (new button% 
                                   [label (list navigate-previous-icon "Step" 'left)] 
                                   [parent navigator] 
                                   [callback (lambda (b e) (navigate-previous))]
                                   [enabled #f]))
        (set! next-button (new button% 
                               [label (list navigate-next-icon "Step" 'right)]
                               [parent navigator]
                               [callback (lambda (b e) (navigate-next))]))
        (set! status-msg (new message% [label ""] [parent navigator] [stretchable-width #t]))))
    
    (define/private (navigate-previous)
      (set! step (sub1 step))
      (send slider set-value step)
      (update-trace-view-backward))
    
    (define/private (navigate-next)
      (set! step (add1 step))
      (send slider set-value step)
      (update-trace-view-forward))
    
    (define/private (set-current-step s)
      (cond
        [(< step s)
         (set! step s)
         (update-trace-view-forward)]
        [(> step s) 
         (set! step s)
         (update-trace-view-backward)]))
    
    (define/public (set-traces trace) 
      (set! traces (map (lambda (t) (trace-struct (first t) (second t) (third t) (fourth t))) trace)))
    
    (define/public (display-traces)
      (let ([logs (map (lambda (t) (format "~a: ~v\n" (syntax->datum (trace-struct-id-stx t)) (trace-struct-value t))) traces)])
        (send log-text display-logs logs #f)))
      
             
    (send view-text set-styles-sticky #f)
    (send view-text lock #t)
    
    (define/public (add-text text)
      (with-unlock view-text
        (send view-text insert text)))
    
    (define/public (add-syntax i #:highlight-color [highlight-color "LightCyan"])
      (with-unlock view-text
        (let ([stx (list-ref function-calls i)]
              [hi-stxs (if (= (add1 i) limit) null (list (list-ref last-app-list (add1 i))))])
          (define display (print-syntax-to-editor stx view-text controller
                                                  (send view-text last-position)))
          (send view-text insert "\n")
          (define range (send/i display display<%> get-range))
          (define offset (send/i display display<%> get-start-position))
          (send/i controller selection-manager<%> set-selected-syntax (new notify-box% (value #f)))
          (send/i display display<%> highlight-syntaxes hi-stxs highlight-color)
          (send display refresh))))

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
      (set! current-marks (continuation-mark-set-first (trace-struct-ccm (list-ref traces n)) 'inspect null))
      (set! function-calls (map first current-marks))
      (send view-text set-var-table (map second current-marks))
      (set! last-app-list (map third current-marks))
      (set! limit (length function-calls))
      (set! step 1)
      (initialize-navigator)
      (update-trace-view-forward))
    
    (define/private (update-trace-view)
      (erase-all)
      (cond 
        [(odd? step) 
         (add-syntax (sub1 step))]
        [else 
         (add-syntax (- step 2))
         (add-text "\n")
         (add-text (make-object image-snip% (make-object bitmap% (collection-file-path "red-arrow.bmp" "icons") 'bmp)))
         (add-text "\n\n")
         (add-syntax (sub1 step))])
      (send status-msg set-label (format "Trace ~a of ~a" step limit)))      
    
    (define/private (update-trace-view-forward)
      (send previous-button enable #t)
      (when (>= step limit) (send next-button enable #f))
      (update-trace-view))
    
    (define/private (update-trace-view-backward)
      (send next-button enable #t)
      (when (= step 1) (send previous-button enable #f))
      (update-trace-view))
    
    ;; Initialize
    (super-new)
    (initialize-view-text)
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 2/3))
    (send split-panel end-container-sequence)))