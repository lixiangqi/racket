#lang racket/base
(require racket/class
         racket/gui/base
         framework
         unstable/class-iop
         mrlib/name-message
         "interface.rkt"
         "syntax-display.rkt"
         "text.rkt"
         "util.rkt" 
         images/compile-time
         images/icons/misc
         (for-syntax racket/base
                     images/icons/control
                     images/icons/style)
         (except-in racket/list range))
(provide make-trace-browser
         trace-struct)

(struct trace-struct (exp-stx value number inspect-stx funs vars apps) #:transparent)

(define (make-trace-browser traces)
  (define frame (new frame%
                     [label "Trace Browser"]
                     [width 800]
                     [height 600]))
  (define widget (new widget% [parent frame]))
  (send widget update-traces traces)
  (send frame show #t))

(define widget%
  (class object%
    (init parent)
    
    (field [traces null]
           [sorted-traces null]
           [indexes null]
           [var-tables (make-hasheq)]
           [function-calls null]
           [last-app-list null]
           [step 1]
           [limit 0]
           [call? #f]
           [sorted? #f])
    
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
                      line-start-position
                      line-end-position
                      find-string-all
                      position-line)
             (super-new)
             
             (define var-logs null)
             (define filter-lines null)
             (define counts (make-hasheq))
             (define mark-num 0)
             (define normal-sd (make-object style-delta% 'change-weight 'normal))
             
             (define/public (set-var-logs l)
               (set! var-logs l))
             
             (define/public (update-logs l)
               (set! var-logs (append var-logs (list l))))
             
             (define/public (get-logs) var-logs)
             
             (define/public (update-counts i count)
               (hash-set! counts i count))
               
             (define/public (display-logs)
               (begin-edit-sequence)
               (lock #f)
               (delete 0 (last-position))
               (for-each (lambda (l) (insert l)) var-logs)
               (lock #t)
               (end-edit-sequence))
             
             (define/private (move-to-view num)
               (set! mark-num num)
               (begin-edit-sequence)
               (lock #f)
               (change-style normal-sd 0 (last-position))
               (change-style bold-sd (line-start-position mark-num)
                                     (line-end-position mark-num))
               (lock #t)
               (end-edit-sequence))
             
             ;; insert filtered text and apply highlighting
             ;; only highlight the first found item in a single line
             (define/private (add-filtered-text filter-lines lines offsets str-length)
               (begin-edit-sequence)
               (lock #f)
               (delete 0 (last-position))
               (for-each
                (lambda (l)
                  (insert (list-ref var-logs l)))
                filter-lines)
               (let ([last -1]
                     [counter 0])
                 (for ([i (in-range (length lines))])
                   (let ([line (list-ref lines i)])
                     (unless (= line last)
                       (let* ([offset (list-ref offsets i)]
                              [start-pos (+ (line-start-position counter) offset)]
                              [end-pos (+ start-pos str-length)])
                         (change-style (search-style-delta "Coral") start-pos end-pos))
                       (if sorted?
                           (set! counter (+ (cdr (list-ref indexes line)) 1))
                           (set! counter (add1 counter)))
                       (set! last line)))))
               (lock #t)
               (end-edit-sequence))
             
             (define/public (filter-logs search-str)
               (cond
                 [(eq? search-str "")
                  (set! filter-lines null)
                  (display-logs)]
                 
                 [sorted?
                  (let* ([found (find-string-all search-str 'forward 0 (last-position))]
                         [lines (map (lambda (p) (position-line p)) found)]
                         [offsets (map (lambda (p l) (- p (line-start-position l))) found lines)]
                         [str-length (string-length search-str)]
                         [tmp null]
                         [trace-size (length sorted-traces)])
                    ; if search for a sorted variable, add all lines of its values
                    (for ([i (in-list (remove-duplicates lines))])
                      (set! tmp (append tmp (list i)))
                      (unless (list-ref sorted-traces i)
                        (let loop ([j (add1 i)])
                          (when (and (< j trace-size) (list-ref sorted-traces j))
                            (set! tmp (append tmp (list j)))
                            (loop (add1 j))))))
                    (set! filter-lines tmp)
                    ; insert filtered text
                    (add-filtered-text filter-lines lines offsets str-length))]
                 
                 [else
                  (let* ([found (find-string-all search-str 'forward 0 (last-position))]
                         [lines (map (lambda (p) (position-line p)) found)]
                         ; offsets for aiding change-style 
                         [offsets (map (lambda (p l) (- p (line-start-position l))) found lines)]
                         [str-length (string-length search-str)])
                    (set! filter-lines (remove-duplicates lines))  
                    (add-filtered-text filter-lines lines offsets str-length))])
                 (erase-all)
                 (label-view-text "No trace selected\n"))
             
             (define/private (search-style-delta color)
               (let ([sd (new style-delta%)])
                 (send sd set-delta-foreground color)
                 sd))
             
             (define/override (on-event evt)
               (let*-values ([(x y) (dc-location-to-editor-location (send evt get-x) (send evt get-y))]
                             [(line) (find-line y)])
                 (case (send evt get-event-type)
                   [(left-down)
                    (cond
                      [sorted?
                       (when (< line (length sorted-traces))
                         (let ([curr (list-ref sorted-traces line)])
                           (when curr
                             (move-to-view line)
                             (update-view-text curr))))]
                      [else
                       (if (null? filter-lines)
                           (when (< line (length var-logs))
                             (move-to-view line)
                             (update-view-text (list-ref traces line)))
                           (when (< line (length filter-lines))
                             (move-to-view line)
                             (update-view-text (list-ref traces (list-ref filter-lines line)))))])]))))))
                 
    (define search-text
      (new (class text%
             (inherit get-text)
             
             (super-new)
               
             (define/augment (after-insert start len)
               (inner (void) after-insert start len)
               (update-str-to-search))
             
             (define/augment (after-delete start len)
               (inner (void) after-delete start len)
               (update-str-to-search))
             
             (define/private (update-str-to-search)
               (send log-text display-logs)
               (send log-text filter-logs (get-text))))))
    
    (define sort-canvas%
      (class name-message%
        
        (super-new (label "Sort"))
        
        (define/private (modify-sorting-order position?)
          (cond
            [position?
             (set! sorted-traces (sort traces < #:key (lambda (x) (syntax-position (trace-struct-exp-stx x)))))
             (let ([start (trace-struct-exp-stx (first sorted-traces))]
                   [counter 0])
               (for ([t (in-list sorted-traces)])
                 (let ([curr (trace-struct-exp-stx t)])
                   (cond 
                     [(eq? curr start)
                      (set! counter (add1 counter))]
                     [else
                      (set! indexes (append indexes (list (cons start counter))))
                      (set! start curr)
                      (set! counter 1)])))
               (set! indexes (append indexes (list (cons start counter))))
               (display-sorted-traces))]
            [else
             (set! sorted? #f)
             (display-traces)]))
          
        (define/override (fill-popup menu reset)
          (make-object menu:can-restore-menu-item% "sort by position in file"
            menu
            (λ (x y)
              (unless sorted?
                (modify-sorting-order #t))))
          (make-object menu:can-restore-menu-item% "sort by log time"
            menu
            (λ (x y)
              (when sorted?
                (modify-sorting-order #f)))))))
    
    (define navigator 'uninitialized-navigator)
    (define previous-button 'uninitialized-previous-button)
    (define next-button 'uninitialized-next-button)
    (define status-msg 'uninitialized-status-msg)
    (define slider-panel 'uninitialized-slider-panel)
    (define slider #f)   
    
    (define main-panel
      (new vertical-panel% [parent parent]))
    (define split-panel
      (new panel:horizontal-dragable% [parent main-panel]))
    
    (define log-panel 
      (new vertical-panel% [parent split-panel]))
    (send log-panel begin-container-sequence)
    (define search-panel
      (new horizontal-panel% 
           [parent log-panel]
           [stretchable-height #f]))
    (new editor-canvas% 
         [parent search-panel] 
         [editor search-text]
         [style '(hide-hscroll hide-vscroll)]
         [vertical-inset 2]
         [line-count 1]
         [stretchable-height #f])
    (new message%
         [label (magnifying-glass-icon #:height (send (send search-text get-dc) get-char-height))]
         [parent search-panel]
         [stretchable-height #f])
    (new editor-canvas% 
         [parent log-panel] 
         [editor log-text] 
         [style '(auto-hscroll)])
    
    (define sort-panel
      (new horizontal-panel%
           [parent log-panel]
           [stretchable-height #f]))
    
    (define sort-canvas
      (new sort-canvas%
         [parent sort-panel]))
    (new message% [label ""] [parent sort-panel] [stretchable-width #t])
             
    (send log-panel end-container-sequence)
      
    (define view-text (new browser-text%))
    (define view-panel (new vertical-panel% [parent split-panel]))
    (define view-canvas (new canvas:color% (parent view-panel) (editor view-text)))
    
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
      (set! status-msg (new message% [label ""] [parent navigator] [stretchable-width #t]))
      (send view-panel change-children (lambda (l) (remove* (list slider-panel navigator) l eq?))))
               
    (define bold-sd (make-object style-delta% 'change-weight 'bold))           
       
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
    
    (define/private (display-sorted-traces)
      (let* ([j 0]
             [size (length indexes)]
             [cur-index (list-ref indexes j)]
             [index-stx (car cur-index)]
             [index-count (cdr cur-index)]
             [tmp null])
        (send log-text set-var-logs null)
        (send log-text update-logs (format "~a (num: ~a)\n" (syntax->datum index-stx) index-count))
        (send log-text update-counts (length tmp) index-count)
        (set! tmp (append tmp (list #f)))
        (for ([i (in-list sorted-traces)])
          (cond
            [(eq? (trace-struct-exp-stx i) index-stx)
             (set! tmp (append tmp (list i)))
             (send log-text update-logs (format "  ~v\n" (trace-struct-value i)))]
            [else
             (set! j (add1 j))
             (when (< j size)
               (set! cur-index (list-ref indexes j))
               (set! index-stx (car cur-index))
               (send log-text update-counts (length tmp) (cdr cur-index))
               (set! tmp (append tmp (list #f i)))
               (send log-text update-logs (format "~a (num: ~a)\n" (syntax->datum (car cur-index)) (cdr cur-index)))
               (send log-text update-logs (format "  ~v\n" (trace-struct-value i))))]))
        (set! sorted-traces tmp)
        (set! sorted? #t))
      (send log-text display-logs))
    
    (define/private (display-traces)
      (let ([logs (map (lambda (t) (format "~a: ~v\n" (syntax->datum (trace-struct-exp-stx t)) (trace-struct-value t))) traces)])
        (send log-text set-var-logs logs)
        (send log-text display-logs)))
    
    (define/public (update-traces t)
      (set! traces t)
      (display-traces))
    
    (send view-text set-styles-sticky #f)
    (send view-text lock #t)
    
    (define/public (add-text text)
      (with-unlock view-text
        (send view-text insert text)))
    
    (define/public (add-syntax i)
      (with-unlock view-text
        (let ([stx (list-ref function-calls i)]
              [hi-stxs (if (> (add1 i) limit) null (list (list-ref last-app-list i)))]
              [highlight-color "LightCyan"])
          (define display (print-syntax-to-editor stx view-text
                                                  (list-ref var-tables i)
                                                  (calculate-columns)
                                                  (send view-text last-position)))
          (send view-text insert "\n")
          (define range (send/i display display<%> get-range))
          (define offset (send/i display display<%> get-start-position))
          (when (and (zero? i) (not call?))
            (set! highlight-color "MistyRose"))
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
    
    (define/private (add-separator)
      (add-text "\n")
      (add-text (make-object image-snip% (make-object bitmap% (collection-file-path "red-arrow.bmp" "icons") 'bmp)))
      (add-text "\n\n"))

    (define/public (erase-all)
      (with-unlock view-text
        (send view-text erase)))
    
    (define/public (get-text) view-text)
    
    (define/private (label-text text label)
      (let ([sd (new style-delta%)])
        (with-unlock text
          (send text erase)
          (send sd set-delta-foreground "gray")
          (send text insert label)
          (send text change-style sd (send text paragraph-start-position 0) 
                                     (send text paragraph-end-position 0)))))
    
    (define/public (label-view-text label)
      (label-text view-text label)
      (when slider
        (send slider-panel delete-child slider)
        (send view-panel change-children (lambda (l) (remove* (list slider-panel navigator) l eq?)))
        (set! slider #f)))
    
    (define/private (update-view-text current-trace)
      (cond
        [(null? (trace-struct-funs current-trace))
         (label-view-text "No associated evaluation steps\n")]
        [else
         (if slider
             (send slider-panel delete-child slider)
             (send view-panel change-children (lambda (l) (append l (list slider-panel navigator)))))
         (let* ([funs (trace-struct-funs current-trace)]
                [vars (trace-struct-vars current-trace)]
                [inspect-stx (trace-struct-inspect-stx current-trace)]
                [apps (append (trace-struct-apps current-trace) (list inspect-stx))])
           (cond
             [inspect-stx
              (set! function-calls (reverse funs))
              (set! var-tables (reverse vars))
              (set! last-app-list (reverse apps))
              (set! call? #f)]
             [else
              (set! function-calls funs)
              (set! var-tables vars)
              (set! last-app-list (rest apps))
              (set! call? #t)])
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
           (update-trace-view-forward))]))
    
    (define/private (update-trace-view)
      (erase-all)
      (cond
        [(odd? step)
         (add-syntax (sub1 step))]
        [else 
         (add-syntax (- step 2))
         (add-separator)
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
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 2/3))
    (label-view-text "No trace selected\n")
    (send split-panel end-container-sequence)))