#lang racket/base
(require racket/class
         racket/gui/base
         framework
         unstable/class-iop
         unstable/gui/notify
         "interface.rkt"
         "syntax-display.rkt"
         "text.rkt"
         "util.rkt" 
         images/compile-time
         (for-syntax racket/base
                     images/icons/control
                     images/icons/style)
         (except-in racket/list range))
(provide make-trace-browser
         trace-struct)

(struct trace-struct (id-stx value number inspect-stx funs vars apps))

(define (make-trace-browser traces)
  (define frame (new frame%
                     [label "Trace Browser"]
                     [width 800]
                     [height 600]))
  (define widget (new widget% [parent frame]))
  (send widget display-traces traces)
  (send frame show #t))

(define widget%
  (class object%
    (init parent)
    
    (field [traces empty]
           [var-tables (make-hasheq)]
           [function-calls empty]
           [last-app-list empty]
           [step 1]
           [limit 0]
           [show? #f])
    
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
                      paragraph-end-position)
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
    (define slider-panel 'uninitialized-slider-panel)
    (define slider 'uninitialized-slider)
    
    ;; Initialize navigator 
    (let ([navigate-previous-icon (compiled-bitmap (step-back-icon #:color run-icon-color #:height (toolbar-icon-height)))]
          [navigate-next-icon (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height)))])
      (set! slider-panel (new horizontal-panel% [parent view-panel] [stretchable-width #f] [stretchable-height #f]))
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
      (set! status-msg (new message% [label ""] [parent navigator] [stretchable-width #t])))
       
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
    
    (define/public (display-traces t)
      (set! traces t)
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
              [hi-stxs (if (= (add1 i) limit) null (list (list-ref last-app-list i)))])
          (define display (print-syntax-to-editor stx view-text
                                                  (calculate-columns)
                                                  (send view-text last-position)))
          (send view-text insert "\n")
          (define range (send/i display display<%> get-range))
          (define offset (send/i display display<%> get-start-position))
          (send/i display display<%> highlight-syntaxes hi-stxs highlight-color)
          (send display refresh))))
    
    (define/private (code-style text)
      (let* ([style-list (send text get-style-list)]
             [style (send style-list find-named-style (editor:get-default-color-style-name))])
        style))
    
    (define/private (calculate-columns)
      (define style (code-style view-text))
      (define char-width (send style get-text-width (send view-canvas get-dc)))
      (let ([admin (send view-text get-admin)]
            [w-box (box 0.0)])
        (send admin get-view #f #f w-box #f)
        (sub1 (inexact->exact (floor (/ (unbox w-box) char-width))))))

    (define/public (erase-all)
      (with-unlock view-text
        (send view-text erase)))
    
    (define/public (get-text) view-text)
    
    (define/public (initialize-view-text)
      (let ([sd (new style-delta%)])
        (with-unlock view-text
          (send sd set-delta-foreground "gray")
          (send view-text insert "No trace selected\n")
          (send view-text change-style sd (send view-text paragraph-start-position 0) 
                                          (send view-text paragraph-end-position 0)))))
    
    (define/private (update-view-text n)
      (cond 
        [show? (send slider-panel delete-child slider)]
        [else
         (send view-panel change-children (lambda (l) (append l (list slider-panel navigator))))
         (set! show? #t)])
      (let ([current-trace (list-ref traces n)])
        (set! function-calls (reverse (trace-struct-funs current-trace)))
        (set! var-tables (reverse (trace-struct-vars current-trace)))
        (set! last-app-list (reverse (append (trace-struct-apps current-trace)
                                             (list (trace-struct-inspect-stx current-trace))))))
      (set! limit (length function-calls))
      (set! step 1)
      (send previous-button enable #t)
      (send next-button enable #t)
      (set! slider (new slider% 
                        [label #f] 
                        [min-value 1] 
                        [max-value limit] 
                        [parent slider-panel] 
                        [style (list 'horizontal 'plain)]
                        [callback (lambda (b e) (set-current-step (send slider get-value)))]))
      (update-trace-view-forward))
    
    (define/private (update-trace-view)
      (erase-all)
      (cond 
        [(odd? step)
         (send view-text set-var-table (list-ref var-tables (sub1 step)))
         (add-syntax (sub1 step))]
        [else 
         (send view-text set-var-table (cons (list-ref var-tables (- step 2))
                                             (list-ref var-tables (sub1 step))))
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
    (send view-panel change-children (lambda (l) (remove* (list slider-panel navigator) l eq?)))
    (send split-panel end-container-sequence)))