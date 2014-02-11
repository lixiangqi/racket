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
         "../trace-util.rkt"
         "controller.rkt"
         images/compile-time
         images/icons/misc
         (for-syntax racket/base
                     images/icons/control
                     images/icons/style)
         (except-in racket/list range))
(provide widget%
         trace-struct)

(struct trace-struct (exp-stx value number label inspect-stx funs vars apps) #:transparent)

(define widget%
  (class object%
    (init parent)
    
    (field [traces null]
           [histories null]
           [subtree null]
           [stack null]
           [sorted-traces null]
           [indexes null]
           [var-tables (make-hasheq)]
           [arg-table null]
           [def-table null]
           [function-calls null]
           [last-app-list null]
           [stack-index 0]
           [step 0]
           [limit 0]
           [call? #f]
           [sorted? #f]
           [explore-stack? #f]
           [controller (new controller%)])
    
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
               (cond
                 [sorted?
                  (let ([counter 0])
                    (for ([line (in-list lines)]
                          [offset (in-list offsets)])
                      (let* ([start-pos (+ (line-start-position counter) offset)]
                             [end-pos (+ start-pos str-length)])
                        (change-style (search-style-delta "Coral") start-pos end-pos))
                      (set! counter (+ (hash-ref counts line) 1))))]
                 [else
                  (let ([last -1]
                        [counter 0])
                    (for ([i (in-range (length lines))])
                      (let ([line (list-ref lines i)])
                        (unless (= line last)
                          (let* ([offset (list-ref offsets i)]
                                 [start-pos (+ (line-start-position counter) offset)]
                                 [end-pos (+ start-pos str-length)])
                            (change-style (search-style-delta "Coral") start-pos end-pos))
                          (set! counter (add1 counter))
                          (set! last line)))))])
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
                    ; remove duplicate items in lines and items not in expression line
                    ; update the corresponding offsets
                    (let ([last -1]
                          [tmp-lines null]
                          [tmp-offsets null])
                      (for ([i (in-range (length lines))])
                        (let ([line (list-ref lines i)])
                          (when (and (not (= line last))
                                     (not (list-ref sorted-traces line)))
                            (set! tmp-lines (append tmp-lines (list line)))
                            (set! tmp-offsets (append tmp-offsets (list (list-ref offsets i))))
                            (set! last line))))
                      (set! lines tmp-lines)
                      (set! offsets tmp-offsets))
                    ; if search for a sorted variable, add all lines of its values
                    (for ([i (in-list lines)])
                      (set! tmp (append tmp (list i)))
                      (let loop ([j (add1 i)])
                        (when (and (< j trace-size) (list-ref sorted-traces j))
                          (set! tmp (append tmp (list j)))
                          (loop (add1 j)))))
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
                             (update-view-text (traced-value-trace (trace-struct-value (list-ref traces line))) #f))
                           (when (< line (length filter-lines))
                             (move-to-view line)
                             (update-view-text (traced-value-trace (trace-struct-value (list-ref traces (list-ref filter-lines line)))) #f)))])]))))))
                 
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
             
             (define/public (update-str-to-search)
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
                   [label (trace-struct-label (first sorted-traces))]
                   [counter 0])
               (for ([t (in-list sorted-traces)])
                 (let ([curr (trace-struct-exp-stx t)])
                   (cond 
                     [(eq? curr start)
                      (set! counter (add1 counter))]
                     [else
                      (set! indexes (append indexes (list (list start counter label))))
                      (set! start curr)
                      (set! label (trace-struct-label t))
                      (set! counter 1)])))
               (set! indexes (append indexes (list (list start counter label))))
               (display-sorted-traces))]
            [else
             (set! sorted? #f)
             (display-traces)])
          (send search-text update-str-to-search))
          
        (define/override (fill-popup menu reset)
          (make-object menu:can-restore-menu-item% "sort by position in file"
            menu
            (λ (x y)
              (unless sorted?
                (modify-sorting-order #t))))
          (make-object menu:can-restore-menu-item% "sort by logging time"
            menu
            (λ (x y)
              (when sorted?
                (modify-sorting-order #f)))))))
    
    (define navigator 'uninitialized-navigator)
    (define previous-button 'uninitialized-previous-button)
    (define next-button 'uninitialized-next-button)
    
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
    
    ;; Initialize navigator 
    (let ([navigate-previous-icon (compiled-bitmap (step-back-icon #:color run-icon-color #:height (toolbar-icon-height)))]
          [navigate-next-icon (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height)))])
      (set! navigator (new horizontal-panel% [parent view-panel] [stretchable-height #f] [alignment '(center center)]))
      
      (set! previous-button (new button% 
                                 [label (list navigate-previous-icon "Step" 'left)] 
                                 [parent navigator] 
                                 [callback (lambda (b e) (navigate-previous))]
                                 [enabled #f]))
      
      (set! next-button (new button% 
                             [label (list navigate-next-icon "Step" 'right)]
                             [parent navigator]
                             [callback (lambda (b e) (navigate-next))]
                             [enabled #f])))
    
    (define view-canvas (new canvas:color% (parent view-panel) (editor view-text)))
               
    (define bold-sd (make-object style-delta% 'change-weight 'bold))
       
    (define/private (navigate-next)
      (cond
        [explore-stack?
         (cond 
           [(= stack-index (length stack))
            (update-view-text subtree #f)]
           [else
            (update-stack-view stack-index)
            (set! stack-index (sub1 stack-index))
            (when (= stack-index 0)
              (send next-button enable #f))])]
        [else
         (update-view-text subtree #f)]))
    
    (define/private (navigate-previous)
      (cond
        [explore-stack?
         (send next-button enable #t)
         (update-stack-view stack-index)
         (set! stack-index (add1 stack-index))
         (when (= stack-index (length stack))
           (send previous-button enable #f))]
        [else
         (set! step (sub1 step))
         (update-trace-view-backward)]))
    
    (define/private (display-sorted-traces)
      (let* ([j 0]
             [size (length indexes)]
             [cur-index (list-ref indexes j)]
             [index-stx (first cur-index)]
             [index-count (second cur-index)]
             [label (third cur-index)]
             [tmp null])
        (send log-text set-var-logs null)
        (if (eq? label "")
            (send log-text update-logs (format "~a (size: ~a)\n" (syntax->datum index-stx) index-count))
            (send log-text update-logs (format "~a: ~a (size: ~a)\n" label (syntax->datum index-stx) index-count)))
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
               (set! index-stx (first cur-index))
               (set! index-count (second cur-index))
               (set! label (third cur-index))
               (send log-text update-counts (length tmp) index-count)
               (set! tmp (append tmp (list #f i)))
               (if (eq? label "")
                   (send log-text update-logs (format "~a (size: ~a)\n" (syntax->datum index-stx) index-count))
                   (send log-text update-logs (format "~a: ~a (size: ~a)\n" label (syntax->datum index-stx) index-count)))
               (send log-text update-logs (format "  ~v\n" (trace-struct-value i))))]))
        (set! sorted-traces tmp)
        (set! sorted? #t))
      (send log-text display-logs))
    
    (define/private (display-traces)
      (let ([logs (map (lambda (t) 
                         (let ([label (trace-struct-label t)])
                           (if (eq? label "")
                               (format "~a = ~v\n" (syntax->datum (trace-struct-exp-stx t)) (traced-value-val (trace-struct-value t)))
                               (format "~a: ~a = ~v\n" label (syntax->datum (trace-struct-exp-stx t)) (trace-struct-value t)))))
                       traces)])
        (send log-text set-var-logs logs)
        (send log-text display-logs)))
    
    (define/public (update-traces t)
      (set! traces t)
      (display-traces))
    
    (define/public (update-trace-table p)
      (set! arg-table (car p))
      (set! def-table (cdr p)))
    
    (send view-text set-styles-sticky #f)
    (send view-text lock #t)
    
    (define/public (add-text text)
      (with-unlock view-text
        (send view-text insert text)))
    
    #;(define/public (add-syntax i)
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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define/public (add-syntax stx hi-stxes arg-values underline? fun stack)
      ;(printf "fun=~a, stack=~a\n" fun stack)
      (with-unlock view-text
        (let ([arg-stxes (hash-ref arg-table (syntax-position stx) (lambda () null))]
              [fun-binding (if (syntax? fun) (list (cons fun stack)) null)])
          (define display (print-syntax-to-editor stx view-text controller this
                                                  (make-hasheq (append (map cons arg-stxes arg-values) fun-binding))
                                                  (calculate-columns)
                                                  (send view-text last-position)))
          (define range (send/i display display<%> get-range))
          (define offset (send/i display display<%> get-start-position))
          (when hi-stxes
            (send/i display display<%> highlight-syntaxes hi-stxes "MistyRose"))
          (when underline?
            (send/i display display<%> underline-syntax stx))
          (for ([a arg-values]
                [s arg-stxes])
            (when (and (dtree? a) (equal? (dtree-label a) 'app))
              (send/i display display<%> underline-syntax s)))
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
        (send view-text erase))
      (send/i controller displays-manager<%> remove-all-syntax-displays)
      (send/i controller selection-manager<%> set-selected-syntax #f))
    
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
      (label-text view-text label))
    
    (define/public (get-trace-result tree)
      (case (dtree-label tree)
        ['app 
         (let ([res (dtree-rtree tree)])
           (if (dtree? res)
               (get-trace-result res)
               (dtree-node res)))]
        ['lf 
         (dtree-node tree)]))
 
    (define/public (update-view-text current-trace replay?)
      (erase-all)
      (unless replay?
        (set! step (add1 step))
        (when (> step 1)
          (send previous-button enable #t))
        (set! histories (append histories (list current-trace))))
      (let ([node (dtree-node current-trace)])
        (cond
          [(equal? (dtree-label current-trace) 'app)
           (let ([fnode (dtree-node (atree-ftree node))]
                 [args (map dtree-node (atree-ptree node))]
                 [res (get-trace-result (dtree-rtree current-trace))])
             (cond
               [(equal? (dtree-label (atree-ftree node)) 'lfl)
                (add-syntax (first (cdr fnode)) (list (third (cdr fnode))) (second (cdr fnode)) #f #f #f)
                
                (add-separator)
                (add-text "(")
                (add-syntax (quasisyntax #,(car fnode)) #f null #f #f #f)
                (add-text " ")
                (map (lambda (t) 
                       (add-syntax (syntax-property (quasisyntax #,(get-trace-result t)) 'has-history t)
                                   #f null (equal? (dtree-label t) 'app) #f #f)
                       (add-text " ")) (atree-ptree node))
                (add-text ")")]
               [else
                (add-syntax (hash-ref def-table (car fnode)) #f (atree-ptree node) #f (cddr fnode) (cadr fnode))
                (add-text "\n")
                (add-text "= ")
                (add-syntax (syntax-property (quasisyntax #,res) 'has-history (dtree-rtree current-trace)) 
                            #f null (equal? (dtree-label (dtree-rtree current-trace)) 'app) #f #f)]))]
          [else
           (printf "else entered!\n")
           (printf "current-trace=~a\n" current-trace)
           (void)])))
    
    (define/public (update-stack-view i)
      (erase-all)
      (with-unlock view-text
        (let* ([current-stack (list-ref stack i)]
               [stx (first current-stack)])
          (define display (print-syntax-to-editor stx view-text controller this
                                                  ((second current-stack))
                                                  (calculate-columns)
                                                  (send view-text last-position)))
          (send view-text insert "\n")
          (define range (send/i display display<%> get-range))
          (define offset (send/i display display<%> get-start-position))
          (send display refresh))))
    
    (define/public (explore-subtree stx-trace)
      (send next-button enable #t)
      (set-explore-stack #f)
      (set! subtree stx-trace))
    
    (define/public (disable-subtree-explore)
      (send next-button enable #f))
    
    (define/public (set-current-stack s)
      (send next-button enable #f)
      (send previous-button enable #t)
      (set-explore-stack #t)
      (set! stack-index 0)
      (set! stack s))
    
    (define/public (set-explore-stack b)
      (set! explore-stack? b))
    
    (define/public (get-explore-stack) explore-stack?)
    
    (define/private (update-trace-view)
      (erase-all)
      (cond
        [(odd? step)
         (add-syntax (sub1 step))]
        [else 
         (add-syntax (- step 2))
         (add-separator)
         (add-syntax (sub1 step))]))
    
    (define/private (update-trace-view-backward)
      (when (= step 1) (send previous-button enable #f))
      (set! histories (take histories (sub1 (length histories))))
      (update-view-text (list-ref histories (sub1 step)) #t))
    
    ;; Initialize
    (super-new)
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 2/3))
    (label-view-text "No trace selected\n")
    (send split-panel end-container-sequence)))