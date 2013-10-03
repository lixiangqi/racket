#lang racket/base

;; DrRacket's debugging tool

(require racket/function
         racket/list
         racket/class
         racket/unit
         racket/contract
         racket/match
         racket/gui
         drscheme/tool
         "marks.rkt"
         mrlib/switchable-button
         mrlib/bitmap-label
         mrlib/close-icon
         "annotator.rkt"
         "load-sandbox.rkt"
         "syntax-traversal.rkt"
         framework
         string-constants
         lang/debugger-language-interface
         images/compile-time
         (for-syntax racket/base
                     racket/class
                     racket/draw
                     images/icons/arrow
                     images/icons/control
                     images/icons/style
                     images/icons/symbol
                     images/icons/tool
                     pict))

(provide tool@)

; QUESTIONS/IDEAS
; what is the right way to deal with macros?
; how can the three tool classes communicate with each other safely

(define-local-member-name debug-callback)

(preferences:set-default 'plt:debug-tool:stack/variable-area
                         9/10 
                         (λ (x) (and (real? x) (<= 0 x 1))))

(define tool@
  (unit 
    (import drscheme:tool^)
    (export drscheme:tool-exports^) 

    (define (phase1)
      (drscheme:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (new switchable-button%
              (label (string-constant debug-tool-button-name))
              (bitmap debug-bitmap)
              (alternate-bitmap small-debug-bitmap)
              (parent parent)
              (callback (λ (button) (send frame debug-callback)))))
       'macro-stepper)
      (drscheme:language:extend-language-interface
       debugger-language<%>
       (lambda (superclass)
         (class* superclass (debugger-language<%>)
           (public debugger:supported?)
           (define (debugger:supported?) #t)
                 (super-instantiate ())))))
    (define phase2 void)
    
    (define (extract-language-level settings)
      (drscheme:language-configuration:language-settings-language settings))
    
    (define (debugger-does-not-work-for? lang)
      (not (send lang debugger:supported?)))
    
    (define (robust-syntax-source stx)
      (and (syntax? stx) (syntax-source stx)))
    
    (define (robust-vector-ref vec idx)
      (if (< idx (vector-length vec))
          (vector-ref vec idx)
          #f))
    
    (define (safe-vector-set! vec idx val)
      (when (< idx (vector-length vec))
        (vector-set! vec idx val))
      (void))
    
    (define (truncate str n)
      (cond [(< (string-length str) n) str]
            [(>= n 3) (string-append
                        (substring str 0 (- n 3))
                        "...")]
            [else (substring str 0 (min n (string-length str)))]))
    
    (define (clean-status s)
      (truncate (regexp-replace* #rx"\n" s " ") 200))
    
    (define (index-of chr str)
      (let loop ([i 0])
        (if (< i (string-length str))
            (if (char=? chr (string-ref str i))
                i
                (loop (add1 i)))
            #f)))
    
    (define (safe-min . args)
      (apply min (filter identity args)))
    
    ;; trim-expr-str: string -> string
    ;; examples:
    ;; short-id                   => short-id
    ;; really-long-identifier     => really-lon...
    ;; (<form>)                   => (<form>)
    ;; (<form> <arg1> ... <argn>) => (<form> ...)
    (define trim-expr-str
      (lambda (str [len 10])
        (let* ([strlen (string-length str)]
               [starts-with-paren (and (> strlen 0)
                                       (char=? (string-ref str 0) #\())]
               [len2 (+ len 4)]
               [trunc-pos (safe-min (index-of #\space str)
                                    (index-of #\newline str)
                                    (and (> strlen len2) len)
                                    strlen)])
          (if (>= trunc-pos strlen)
              str
              (string-append
               (substring str 0 trunc-pos)
               (if starts-with-paren
                   " ...)"
                   " ..."))))))
    
    (define (average . values)
      (/ (apply + values) (length values)))
    
    (define (truncate-value v size depth)
      (cond
        [(zero? depth) '...]
        [(and (string? v)
              (> (string-length v) size))
         (string-append (substring v 0 size) "...")]
        [(list? v)
         (let* ([len (length v)]
                [res (build-list (min size len)
                                 (lambda (i) (truncate-value (list-ref v i) size (sub1 depth))))])
           (if (> len size) (append res (list '...)) res))]
        [(vector? v)
         (build-vector (min size (vector-length v))
                       (lambda (i)
                         (if (and (= i (sub1 size))
                                  (> size (vector-length v)))
                             '...
                             (truncate-value (vector-ref v i) size (sub1 depth)))))]
        [(bytes? v)
         (if (> (bytes-length v) size)
             (bytes-append (subbytes v 0 size) #"...")
             v)]
        [else v]))
    
    (define filename->defs
      (lambda (source [default #f])
        (let/ec k
          (cond
            [(is-a? source editor<%>) source]
            [else
             (send (group:get-the-frame-group) for-each-frame
                   (lambda (frame)
                     (and (is-a? frame drscheme:unit:frame<%>)
                          (let* ([defss (map (lambda (t) (send t get-defs)) (send frame get-tabs))]
                                 [defs (findf (lambda (d) (send d port-name-matches? source)) defss)])
                            (and defs (k defs))))))
             default]))))
    
    (define (debug-definitions-text-mixin super%)
      (class super%
        
        (inherit dc-location-to-editor-location
                 editor-location-to-dc-location
                 invalidate-bitmap-cache
                 begin-edit-sequence
                 end-edit-sequence
                 get-canvas
                 get-top-level-window
                 get-filename
                 get-tab)
        
        (define mouse-over-pos #f)
        (super-instantiate ())
        
        (define/augment (on-delete start len)
          (begin-edit-sequence)
          (let ([breakpoints (send (get-tab) get-breakpoints)]
                [shifts empty])
            (hash-for-each
             breakpoints
             (lambda (pos status)
               (cond
                 ; deletion after breakpoint: no effect
                 [(<= pos start)]
                 ; deletion of breakpoint: remove from table
                 [(and (< start pos)
                       (<= pos (+ start len)))
                  (hash-remove! breakpoints pos)]
                 ; deletion before breakpoint: shift breakpoint
                 [(> pos (+ start len))
                  (hash-remove! breakpoints pos)
                  (set! shifts (cons (cons (- pos len) status) shifts))])))
            (for-each (lambda (p) (hash-set! breakpoints (car p) (cdr p)))
                      shifts))
          (inner (void) on-delete start len))
        
        (define/augment (after-delete start len)
          (inner (void) after-delete start len)
          (when (send (get-tab) debug?)
            (send (get-tab) hide-debug))
          (end-edit-sequence))
        
        (define/augment (on-insert start len)
          (begin-edit-sequence)
          (let ([breakpoints (send (get-tab) get-breakpoints)]
                [shifts empty])
            (hash-for-each
             breakpoints
             (lambda (pos status)
               (when (< start pos)
                 ;; text inserted before this breakpoint, so shift
                 ;; the breakpoint forward by <len> positions
                 (hash-remove! breakpoints pos)
                 (set! shifts (cons (cons (+ pos len) status) shifts)))))
            ;; update the breakpoint locations
            (for-each (lambda (p) (hash-set! breakpoints (car p) (cdr p)))
                      shifts))
          (inner (void) on-insert start len))
        
        (define/augment (after-insert start len)
          (inner (void) after-insert start len)
          (when (send (get-tab) debug?)
            (send (get-tab) hide-debug))
          (end-edit-sequence))
        
        ;; lookup id in the given set of stack frames;
        ;; if that fails, try the top-level environment
        ;; invokes sk on success, fk on failure
        (define/private (lookup-var id frames sk fk)
          (cond
            [(and id frames (lookup-first-binding
                             (lambda (id2) (free-identifier=? id id2))
                             frames (lambda () #f)))
             => (lambda (binding)
                  (sk (mark-binding-value binding)
                      (lambda (v) (mark-binding-set! binding v))))]
            [(and id (send (get-tab) lookup-top-level-var
                           id (lambda () #f)))
             => (lambda (tlb)
                  (sk (tlb) tlb))]
            [else (fk)]))
        
        ;; mouse-event -> (or (values #f #f) (values pos editor))
        (define/private (get-pos/text event)
          (let ([event-x (send event get-x)]
                [event-y (send event get-y)]
                [on-it? (box #f)])
            (let loop ([editor this])
              (let-values ([(x y) (send editor dc-location-to-editor-location
                                        event-x event-y)])
                (cond
                  [(is-a? editor text%)
                   (let ([pos (send editor find-position x y #f on-it?)])
                     (cond
                       [(not (unbox on-it?)) (values #f #f)]
                       [else
                        (let ([snip (send editor find-snip pos 'after-or-none)])
                          (if (and snip
                                   (is-a? snip editor-snip%))
                              (loop (send snip get-editor))
                              (values pos editor)))]))]
                  [(is-a? editor pasteboard%)
                   (let ([snip (send editor find-snip x y)])
                     (if (and snip
                              (is-a? snip editor-snip%))
                         (loop (send snip get-editor))
                         (values #f #f)))]
                  [else (values #f #f)])))))
        
        ;; text% start -> (values left top right bottom)
        ;; (four numbers that indicate the locations in pixels of the
        ;;  box bounding the text between start and end
        (define/private (find-char-box text pos)
          (define start-pos (max 0 (- pos 1)))
          (define end-pos (+ start-pos 1))
          (let ([xlb (box 0)]
                [ylb (box 0)]
                [xrb (box 0)]
                [yrb (box 0)])
            (send text position-location start-pos xlb ylb #t)
            (send text position-location end-pos xrb yrb #f)
            (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location
                                                 (unbox xlb) (unbox ylb))]
                          [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                          [(xr-off yr-off) (send text editor-location-to-dc-location
                                                 (unbox xrb) (unbox yrb))]
                          [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
              (cond
                [(= (send text position-line start-pos)
                    (send text position-line end-pos))
                 (values xl yl xr yr)]
                [else
                 ;; in this case, the open paren we want to draw on top of is on
                 ;; a different line from the operator following it, so we just 
                 ;; give ourselves a little space and draw something, instead of
                 ;; returning strange results (and possibly crashing)
                 (values xl yl (+ 10 xl) (+ yl 10))]))))
        
        (define/private (render v)
          (send (get-tab) render v))
        
        ;; mouse-event% integer -> ()
        ;; handles a right-click on a position that's not a breakable paren
        (define/private (debugger-handle-right-click-non-breakable event pos)
          (let* ([frames (send (get-tab) get-stack-frames)]
                 [pos-vec (send (get-tab) get-pos-vec)]
                 [id (robust-vector-ref pos-vec pos)])
            (unless (lookup-var
                     id frames
                     (lambda (val wr)
                       (let ([id-sym (syntax-e id)]
                             [menu (make-object popup-menu% #f)])
                         (make-object menu-item%
                           (clean-status (format "Print value of ~a to console" id-sym))
                           menu
                           (lambda (item evt)
                             (send (get-tab) print-to-console (format "~a = ~s" id-sym val))))
                         (make-object menu-item% (format "(set! ~a ...)" id-sym) menu
                           (lambda (item evt)
                             (let* ([tmp (get-text-from-user
                                          (format "New value for ~a" id-sym) #f #f
                                          (format "~a" val))])
                               (when tmp
                                 (let/ec k
                                   (wr (with-handlers
                                           ([exn:fail?
                                             (lambda (exn)
                                               (message-box
                                                "Debugger Error"
                                                (format "The following error occurred: ~a"
                                                        (exn-message exn)))
                                               (k))])
                                         (read (open-input-string tmp)))))))))
                         (send (get-canvas) popup-menu menu
                               (+ 1 (inexact->exact (floor (send event get-x))))
                               (+ 1 (inexact->exact (floor (send event get-y)))))
                         #t))
                     (lambda () #f))
              (super on-event event))))
        
        (define/private (debugger-handle-right-click-breakable event breakpoints pos break-status)
          (let ([menu (make-object popup-menu% #f)])
            (make-object menu-item%
              (if break-status
                  "Remove pause at this point"
                  "Pause at this point")
              menu
              (lambda (item evt)
                (hash-set! breakpoints pos (not break-status))
                (invalidate-bitmap-cache)))
            (let ([pc (send (get-tab) get-pc)])
              (if (and pc (= pos pc))
                  (let* ([stat (send (get-tab) get-break-status)]
                         [f (get-top-level-window)]
                         [rendered-value
                          (if (cons? stat)
                              (if (= 2 (length stat))
                                  (render (cadr stat))
                                  (format "~s" (cons 'values 
                                                     (map (lambda (v) (render v)) (rest stat)))))
                              "")])
                    (when (cons? stat)
                      (make-object menu-item%
                        "Print return value to console" menu
                        (lambda _ (send (get-tab) print-to-console
                                        (string-append "return val = " rendered-value)))))
                    (when (not (eq? stat 'break))
                      (make-object menu-item%
                        (if (cons? stat)
                            "Change return value..."
                            "Skip expression...")
                        menu
                        (lambda (item evt)
                          (let ([tmp (get-text-from-user "Return value" #f)])
                            (when tmp
                              (let/ec k
                                (send (get-tab) set-break-status
                                      (cons 'exit-break
                                            (call-with-values
                                             (lambda ()
                                               (with-handlers
                                                   ([exn:fail?
                                                     (lambda (exn)
                                                       (message-box
                                                        "Debugger Error"
                                                        (format "An error occurred: ~a" (exn-message exn))
                                                        #f '(ok))
                                                       (k))])
                                                 (read (open-input-string tmp))))
                                             list)))
                                (invalidate-bitmap-cache))))))))
                  (make-object menu-item%
                    "Continue to this point"
                    menu
                    (lambda (item evt)
                      (hash-set!
                       breakpoints pos
                       (lambda () (hash-set! breakpoints pos break-status) #t))
                      (invalidate-bitmap-cache)
                      (when (send (get-tab) get-stack-frames)
                        (send (get-tab) resume))))))
            (send (get-canvas) popup-menu menu
                  (+ 1 (inexact->exact (floor (send event get-x))))
                  (+ 1 (inexact->exact (floor (send event get-y)))))))
        
        (define/private (debugger-handle-right-click event breakpoints)
          (let-values ([(pos text) (get-pos/text event)])
            (if (and pos text)
                (let* ([pos (add1 pos)]
                       [break-status (hash-ref breakpoints pos (lambda () 'invalid))])
                  (match break-status
                    [(or #t #f (? procedure?))
                     (debugger-handle-right-click-breakable event breakpoints pos break-status)]
                    ['invalid
                     (debugger-handle-right-click-non-breakable event pos)]))
                (super on-event event))))
        
        (define/private (debugger-handle-event event)
          (let ([breakpoints (send (get-tab) get-breakpoints)])
            (cond
              [(send event leaving?)
               (when mouse-over-pos
                 (set! mouse-over-pos #f)
                 (invalidate-bitmap-cache))
               (super on-event event)]
              [(or (send event moving?)
                   (send event entering?))
               (let-values ([(pos text) (get-pos/text event)])
                 (when (and pos text)
                   (let ([pos (add1 pos)])
                     (cond
                       ;; mouse on breakable pos and hasn't moved significantly
                       [(eq? pos mouse-over-pos)]
                       ;; mouse on new breakable pos
                       [(not (eq? (hash-ref
                                   breakpoints pos (lambda () 'invalid)) 'invalid))
                        (set! mouse-over-pos pos)
                        (invalidate-bitmap-cache)]
                       ;; moved off breakable pos
                       [mouse-over-pos
                        (set! mouse-over-pos #f)
                        (invalidate-bitmap-cache)])
                     (let* ([frames (send (get-tab) get-stack-frames)]
                            [pos-vec (send (get-tab) get-pos-vec)]
                            [id (robust-vector-ref pos-vec pos)]
                            ;; Try to look up the identifier and render its value.  If either
                            ;; of these steps fails, just draw an empty string in the status bar.
                            [rendered
                             (lookup-var
                              id (list-tail frames (send (get-tab) get-frame-num))
                                          ;; id found
                                          (lambda (val _)
                                            (cond
                                              [(render val) => (lambda (str)
                                                                 (string-append
                                                                  (symbol->string (syntax-e id)) " = " str))]
                                              [else ""]))
                                          ;; id not found
                                          (lambda () ""))])
                       (send (get-tab) set-mouse-over-msg (clean-status rendered))))))
               (super on-event event)]
              [(send event button-down? 'right)
               (debugger-handle-right-click event breakpoints)]
              [else (super on-event event)])))
          
        (define/override (on-event event)
          (if (send (get-tab) debug?)
              (debugger-handle-event event)
              (super on-event event)))
        
        (define/override (on-paint before dc left top right bottom dx dy draw-caret)
          (super on-paint before dc left top right bottom dx dy draw-caret)
          (when (and (send (get-tab) debug?) (not before))
            ;; render breakpoints
            (let ([breakpoints (send (get-tab) get-breakpoints)])
              (hash-for-each
               breakpoints
               (lambda (pos enabled?)
                 (when (and (>= pos 0) (or enabled? (and mouse-over-pos (= mouse-over-pos pos))))
                   (let*-values ([(xl yl xr yr) (find-char-box this pos)]
                                 [(diameter) (- xr xl)]
                                 [(yoff) (/ (- yr yl diameter) 2)])
                     (let ([op (send dc get-pen)]
                           [ob (send dc get-brush)])
                       (case enabled?
                         [(#t) (send dc set-pen bp-pen)
                               (send dc set-brush bp-brush)]
                         [(#f) (send dc set-pen bp-mo-pen)
                               (send dc set-brush bp-mo-brush)]
                         [else (send dc set-pen bp-tmp-pen)
                               (send dc set-brush bp-tmp-brush)])
                       (send dc draw-ellipse (+ xl dx) (+ yl dy yoff) diameter diameter)
                       (send dc set-pen op)
                       (send dc set-brush ob)))))))
            ;; mark the boundaries of the current stack frame
            ;; unless we're at the end of the expression and looking at the top frame,
            ;; in which case just mark the current location
            (let* ([frame-defs (send (get-tab) defs-containing-current-frame)]
                   [pos (send (get-tab) get-current-frame-endpoints)]
                   [start (and pos (first pos))]
                   [end (and pos (second pos))]
                   [frame-num (send (get-tab) get-frame-num)]
                   [break-status (send (get-tab) get-break-status)])
              (when (and (eq? frame-defs this) start end)
                (let*-values ([(xl yl xr yr) (find-char-box this start)]
                              [(ym) (average yl yr)]
                              [(xa ya xb yb) (find-char-box this end)]
                              [(diameter) (- xb xa)]
                              [(yoff) (/ (- yb ya diameter) 2)]
                              [(ym2) (average ya yb)])
                  (let ([op (send dc get-pen)]
                        [ob (send dc get-brush)])
                    (cond
                      [(and (zero? frame-num)
                            (eq? break-status 'error))
                       (send dc set-pen pc-err-pen)
                       (send dc set-brush pc-err-brush)]
                      [(and (zero? frame-num)
                            (eq? break-status 'break))
                       (send dc set-pen pc-brk-pen)
                       (send dc set-brush pc-brk-brush)]
                      [(zero? frame-num)
                       (send dc set-pen pc-pen)
                       (send dc set-brush pc-brush)]
                      [else
                       (send dc set-pen pc-up-stack-pen)
                       (send dc set-brush pc-up-stack-brush)])
                    (unless (and (zero? frame-num) (cons? break-status))
                      ;; mark the beginning of the expression with a triangle
                      (send dc draw-polygon (list (make-object point% xl yl)
                                                  (make-object point% xl yr)
                                                  (make-object point% xr ym)) dx dy))
                    (if (and (zero? frame-num) (cons? break-status))
                        ;; top frame, end: mark the end of the expression with a triangle
                        (send dc draw-polygon (list (make-object point% xa ya)
                                                    (make-object point% xa yb)
                                                    (make-object point% xb ym2)) dx dy)
                        ;; otherwise: make the end of the expression with a circle
                        (send dc draw-ellipse (+ xa dx) (+ ya dy yoff) diameter diameter))
                    (send dc set-pen op)
                    (send dc set-brush ob)))))))
        
        (define/augment (after-set-next-settings s)
          (let ([tlw (get-top-level-window)])
            (when tlw
              (send tlw check-current-language-for-debugger)))
          (inner (void) after-set-next-settings s))))
    
    ;; pen and brush for drawing a breakpoint
    (define bp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define bp-brush (send the-brush-list find-or-create-brush "red" 'solid))
    ;; pen and brush for marking a location that could have a breakpoint installed
    (define bp-mo-pen (send the-pen-list find-or-create-pen "darkgray" 1 'solid))
    (define bp-mo-brush (send the-brush-list find-or-create-brush "tomato"
                              'solid))
    ;; pen and brush for marking a conditional breakpoint
    (define bp-tmp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define bp-tmp-brush (send the-brush-list find-or-create-brush "yellow"
                               'solid))
    ;; pen and brush for drawing the normal execution location
    (define pc-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-brush (send the-brush-list find-or-create-brush "forestgreen" 'solid))
    ;; pen and brush for marking the expression when not at the top of the stack
    (define pc-up-stack-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-up-stack-brush (send the-brush-list find-or-create-brush "lightgreen" 'solid))
    ;; pen and brush for marking the location when there's an an error
    (define pc-err-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-err-brush (send the-brush-list find-or-create-brush "red" 'solid))
    ;; pen and brush for marking the location following a break
    (define pc-brk-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-brk-brush (send the-brush-list find-or-create-brush "gray" 'solid))
    
    (define (debug-interactions-text-mixin super%)
      (class super%
        
        (inherit run-in-evaluation-thread
                 get-user-thread)
        
        (super-instantiate ())
        
        (define debugged-thread #f)
        (define tab #f)
        (define/public (get-tab) tab)
        (define/public (set-tab t) (set! tab t))
        
        ;; Returns whether the evaluation thread has been killed.
        (define/public (evaluation-thread-dead?)
          (cond
            [(get-user-thread) => thread-dead?]
            [else #t]))
        
        (define/override (kill-evaluation)
          (super kill-evaluation)
          (when (get-tab)
            ;; Remove any markings indicating that the program is suspended.
            (send (get-tab) resume-gui)))
        
        (define/private (stx-source->breakpoints src)
          (send (send (or (and src (filename->defs src)) this) get-tab) get-breakpoints))
        
        (define/private (stx-source->pos-vec src)
          (send (send (or (and src (filename->defs src)) this) get-tab) get-pos-vec))
        
        ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
        ;; adds debugging information to `sexp' and calls `oe'
        (define/private (make-debug-eval-handler oe break? break-before break-after)
          (lambda (orig-exp)
            (cond
              [(compiled-expression? (if (syntax? orig-exp)
                                         (syntax-e orig-exp)
                                         orig-exp))
               (oe orig-exp)]
              [else
               (define exp (if (syntax? orig-exp)
                               orig-exp
                               (namespace-syntax-introduce
                                (datum->syntax #f orig-exp))))
               (when (eq? (filename->defs (and (syntax? orig-exp)
                                               (syntax-source orig-exp)))
                          (send (get-tab) get-defs))
                 (traverse-stx orig-exp)
                 ;(printf "pos-table=~a\n" (get-pos-table))
                 
                 )
               
               (define top-e (expand-syntax-to-top-form exp))
               (define fn (and (syntax? orig-exp)
                               (let ([src (syntax-source orig-exp)])
                                 (and (path? src)
                                      src))))
               (cond
                 [(or (eq? (filename->defs (and (syntax? orig-exp)
                                                (syntax-source orig-exp)))
                           (send (get-tab) get-defs))
                      (annotate-this-module? fn))
                  (parameterize ([current-eval oe])
                   (eval/annotations
                    top-e
                    ; annotate-module?
                    (lambda (fn m)
                      (annotate-this-module? fn))
                    ; annotator
                    (lambda (stx)
                      (define source (syntax-source stx))
                      (define breakpoints (stx-source->breakpoints source))
                      (define pos-vec (stx-source->pos-vec source))
                      (define-values (annotated break-posns)
                        (annotate-for-single-stepping
                         (expand-syntax stx)
                         break? break-before break-after
                         ; record-bound-identifier
                         (lambda (type bound binding)
                           (cond
                             [(filename->defs (robust-syntax-source bound))
                              =>
                              (lambda (defs)
                                (let ([pos-vec (send (send defs get-tab) get-pos-vec)])
                                  (let loop ([i 0])
                                    (when (< i (syntax-span bound))
                                      (safe-vector-set! pos-vec (+ i (syntax-position bound))
                                                        binding)
                                      (loop (add1 i))))))]
                             [else (void)]))
                         ; record-top-level-identifier
                         (lambda (mod var rd/wr)
                           ; filename->defs should succeed unless a slave tab gets closed
                           (cond
                             [(filename->defs (robust-syntax-source var))
                              =>
                              (lambda (defs)
                                (send (send defs get-tab)
                                      add-top-level-binding var rd/wr))]
                             [else (void)]))
                         ; record-log
                         (lambda (id val num cm)
                           (cond 
                             [(filename->defs (robust-syntax-source id))
                              =>
                              (lambda (defs)
                                (send (send defs get-tab)
                                      update-logs id val num cm))]
                             [else (void)]))
                         ; record-env
                         (lambda (expr binding let?)
                           (cond
                             [(filename->defs (robust-syntax-source expr))
                              =>
                              (lambda (defs)
                                (send (send defs get-tab)
                                      add-env-binding binding let?))]
                             [else (void)]))
                         source))
                      (hash-for-each
                       breakpoints
                       (lambda (pos status)
                         ; possible efficiency problem for large files with many breakpoints
                         (when (and (syntax-position stx)
                                    (>= pos (syntax-position stx))
                                    (< pos (+ (syntax-position stx) (syntax-span stx)))
                                    (not (memq pos break-posns)))
                           (hash-remove! breakpoints pos))))
                      (for ([posn (in-list break-posns)]) 
                        (hash-set!
                         breakpoints posn
                         (hash-ref breakpoints posn (lambda () #f))))
                      annotated)))]
                 [else (oe top-e)])])))
        
        (define/private (annotate-this-module? fn)
          (cond
            [(filename->defs fn)
             =>
             ; fn is loaded into defs
             (lambda (defs)
               (let ([extern-tab (send defs get-tab)]
                     [this-tab (get-tab)])
                 (case (if (or (not (send extern-tab debug?))
                               (eq? this-tab (send extern-tab get-master)))
                           (message-box
                            "Debugging Multi-File Program"
                            (format "Debug ~a?" fn)
                            #f
                            '(yes-no))
                           (message-box
                            "Debugging Multi-File Program"
                            (format "~a is already involved in a debugging session." fn)
                            #f
                            '(ok)))
                   [(yes)
                    ; set tab up with shared data from the master tab
                    (send extern-tab prepare-execution #t)
                    (send this-tab add-slave extern-tab)
                    (call-with-values
                     (lambda () (send this-tab get-shared-data))
                     (lambda vals (send extern-tab set-shared-data . vals)))
                    #t]
                   [(no ok)
                    (send extern-tab prepare-execution #f)
                    #f])))]
            ; fn is not open, so don't try to debug it
            [else #f]))
        
        (define/override (reset-console)
          (super reset-console)
          (let ([tab (get-tab)])
            (when (and tab (send tab debug?))
              (let ([breakpoints (send tab get-breakpoints)])
                (run-in-evaluation-thread
                 (lambda ()
                   ;(print-struct #t)
                   (let ([self (current-thread)]
                         [oeh (uncaught-exception-handler)]
                         [err-hndlr (error-display-handler)])
                     (set! debugged-thread self)
                     (error-display-handler
                      (lambda (msg exn)
                        (err-hndlr msg exn)
                        (when (and (eq? self (current-thread)) (exn:fail? exn))
                              (send (get-tab) suspend oeh
                                    (continuation-mark-set->list (exn-continuation-marks exn) debug-key)
                                    'error)))) ; this breaks the buttons because it looks like we can resume
                     (current-eval
                      (make-debug-eval-handler
                       (current-eval)
                       ; break? -- curried to avoid looking up defs from source each time
                       (lambda (src)
                         (let* ([defs (filename->defs src)]
                                [src-tab (if defs (send defs get-tab) (get-tab))]
                                [breakpoints (if src
                                                 (send src-tab get-breakpoints)
                                                 breakpoints)]
                                [single-step? (send tab get-single-step-box)]
                                [closed? (send src-tab get-closed-box)])
                           (lambda (pos)
                             (and (not (unbox closed?))
                                  (or (unbox single-step?)
                                      (let ([bp (hash-ref breakpoints pos #f)])
                                        (if (procedure? bp)
                                            (bp)
                                            bp)))))))
                       ; break-before
                       (lambda (top-mark ccm)
                         (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                         #;(let ([expr (mark-source top-mark)]
                               [top ""])
                           (cond
                             ; should succeed unless the user closes a slave tab during debugging
                             [(and expr (filename->defs (syntax-source expr)))
                              => (lambda (defs)
                                   (set! top (trim-expr-str
                                    (if (syntax-position expr)
                                        (send defs get-text
                                              (sub1 (syntax-position expr))
                                              (+ -1 (syntax-position expr) (syntax-span expr)))
                                        "??")
                                    15)))]
                             ["??"])
                           (printf "top-mark: ~a\n" top))
                           
                           
                           
                           
                           ;---------
                           (send (get-tab) suspend oeh (cons top-mark debug-marks) 'entry-break)))
                       ; break-after
                       (case-lambda
                         [(top-mark ccm val)
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (car (send (get-tab) suspend oeh (cons top-mark debug-marks)
                                       (list 'exit-break val))))]
                         [(top-mark ccm . vals) 
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (apply values
                                   (send (get-tab) suspend oeh (cons top-mark debug-marks)
                                         (cons 'exit-break vals))))])))
                     (uncaught-exception-handler
                      (lambda (exn)
                        (if (and (exn:break? exn) (send (get-tab) suspend-on-break?))
                            (let ([marks (exn-continuation-marks exn)]
                                  [cont (exn:break-continuation exn)])
                              (send (get-tab) suspend oeh (continuation-mark-set->list marks debug-key) 'break)
                              (cont))
                            (oeh exn)))))))))))))
    
    (define (debug-tab-mixin super%)
      (class super%
        
        (inherit get-defs
                 get-ints
                 get-frame
                 is-running?)
        
        (field [breakpoints (make-hash)]
               [suspend-sema (make-semaphore 1)]
               [resume-ch (make-channel)]
               [in-user-ch (make-channel)]
               [want-suspend-on-break? #f]
               [want-debug? #f]
               [master this]
               [slaves empty]
               [closed? (box #f)]
               [stack-frames (box #f)]
               [frame-num (box 0)]
               [break-status (box #f)]
               [current-language-settings #f]
               [pos-vec (vector #f)]
               [single-step? (box #t)]
               [top-level-bindings empty]
               [env-bindings empty]
               [log-pairs empty]
               [outmost-cms empty]
               [id-counts (make-hasheq)]
               [control-panel empty])
        
        (define/public (debug?) want-debug?)
        (define/public (get-master) master)
        (define/public (add-slave s)
          (set! slaves (cons s slaves)))
        (define/public (get-closed-box) closed?)
        (define/public suspend-on-break?
          (case-lambda
            [() want-suspend-on-break?]
            [(v) (set! want-suspend-on-break? v)]))
        (define/public (get-stack-frames)
          (unbox stack-frames))
        (define/public (get-outmost-cms) outmost-cms)
        (define/public (get-pos-vec) pos-vec)
        (define/public (get-breakpoints) breakpoints)
        (define/public (get-break-status) (unbox break-status))
        (define/public (get-frame-num) (unbox frame-num))
        
        (define/public (set-shared-data bs sf sema res-ch usr-ch step? frame m)
          (set! break-status bs)
          (set! stack-frames sf)
          (set! suspend-sema sema)
          (set! resume-ch res-ch)
          (set! in-user-ch usr-ch)
          (set! single-step? step?)
          (set! frame-num frame)
          (set! master m))
        
        (define/public (get-shared-data)
          (values break-status stack-frames suspend-sema resume-ch in-user-ch single-step? frame-num master))
        
        (define/public (get-single-step-box) single-step?)
        (define/public (set-single-step?! v) (set-box! single-step? v))
        (define/public (set-break-status stat) (set-box! break-status stat))
        
        (define/public (add-top-level-binding var rd/wr)
          (set! top-level-bindings (cons (cons var rd/wr) top-level-bindings)))
        
        (define/public (update-logs id-stx val num cm)
          (let* ([pos (syntax-position id-stx)]
                 [count (hash-ref id-counts pos (lambda () 0))])
            (when (< count num)
              (hash-set! id-counts pos (add1 count))
              (let* ([trimmed-exprs
                      (map (lambda (frame)
                             (let ([expr (mark-source frame)])
                               (cond 
                                 [(and expr (filename->defs (syntax-source expr)))
                                  => (lambda (defs)
                                       (trim-expr-str
                                        (if (syntax-position expr)
                                            (send defs get-text
                                                  (sub1 (syntax-position expr))
                                                  (+ -1 (syntax-position expr) (syntax-span expr)))
                                            "??")
                                        15))]
                                 ["??"])))
                           cm)]
                     [last-proc (last cm)]
                     [last-trimmed-expr (last trimmed-exprs)]
                     [quad (list id-stx val trimmed-exprs cm)])
                (if (empty? outmost-cms)
                    (begin
                      (set! outmost-cms (list (list last-proc last-trimmed-expr)))
                      (set! log-pairs (list (list quad))))
                    (if (eq? last-proc (car (last outmost-cms)))
                        (set! log-pairs (append (take log-pairs (sub1 (length log-pairs)))
                                                (list (append (last log-pairs) (list quad)))))
                        (begin
                          (set! outmost-cms (append outmost-cms (list (list last-proc last-trimmed-expr))))
                          (set! log-pairs (append log-pairs (list (list quad)))))))
                (update-trace-editor)))))
        
        (define/public (update-trace-editor)
          (if (empty? outmost-cms)
              (send (get-frame) clear-trace-editor)
              (begin
                (send (send (get-frame) get-sort-button) enable #t)
                (send (send (get-frame) get-call-editor) display-call-proc (map second outmost-cms) #f #f)
                (send (send (get-frame) get-output-editor) set-var-stack-exprs log-pairs)
                (send (send (get-frame) get-view-canvas) set-clear-flag #t)
                (send (send (get-frame) get-view-canvas) refresh))))
        
        (define/public (add-env-binding binding let?)
          ;(printf "add-binding: ~a\n" binding)
          (if let?
              (void)
              (void)
              #;(printf "binding: ~a\n" binding)))
        
        (define/public (lookup-top-level-var var failure-thunk)
          (let loop ([bindings top-level-bindings])
            (cond
              [(empty? bindings) (failure-thunk)]
              [(or (bound-identifier=? var (caar bindings))
                   (free-identifier=? var (caar bindings))) (cdar bindings)]
              [else (loop (rest bindings))])))
        
        (define/public (move-to-frame the-frame-num)
          (set-box! frame-num the-frame-num)
          (suspend-gui (get-stack-frames) (get-break-status) #t #t))
        
        (define/public (resume)
          (let ([v (get-break-status)])
            ;; We should be suspended here, so the user thread should be waiting for a value
            ;; on resume-ch.  However, we set a timeout to guard against cases where
            ;; the user thread gets interrupted or killed unexpectedly.
            (when (sync/timeout 1 (channel-put-evt resume-ch (and (pair? v) (cdr v))))
              (resume-gui))))
        
        (define/public (set-mouse-over-msg msg)
          (send (get-frame) set-mouse-over-msg msg))
        
        (define/public (defs-containing-pc)
          (let ([stack-frames (get-stack-frames)])
            (and (cons? stack-frames)
                 (let* ([src-stx (mark-source (first stack-frames))]
                        [source (syntax-source src-stx)])
                   (if source
                       (filename->defs source)
                       (get-defs))))))
        
        (define/public (defs-containing-current-frame)
          (let ([stack-frames (get-stack-frames)])
            (and (cons? stack-frames)
                 (let* ([src-stx (mark-source (list-ref stack-frames (get-frame-num)))]
                        [source (syntax-source src-stx)])
                   (if source
                       (filename->defs source)
                       (get-defs))))))

        (define/public (get-pc)
          (let ([stack-frames (get-stack-frames)])
            (and (cons? stack-frames)
                 (let* ([src-stx (mark-source (first stack-frames))]
                        [start (syntax-position src-stx)]
                        [end (and start (+ start (syntax-span src-stx) -1))])
                   (if (cons? (get-break-status))
                       end
                       start)))))
        
        (define/public (get-frame-endpoints frame-num)
          (let ([stack-frames (get-stack-frames)])
            (and (cons? stack-frames)
                 (let* ([src-stx (mark-source (list-ref stack-frames frame-num))]
                        [start (syntax-position src-stx)]
                        [end (and start (+ start (syntax-span src-stx) -1))])
                   (list start end)))))
        
        (define/public (get-current-frame-endpoints)
          (get-frame-endpoints (get-frame-num)))
        
        (define/private (do-in-user-thread thunk)
          (if (get-break-status)
              ;; The evaluation thread is suspended, so it should be waiting for thunks
              ;; to arrive on in-user-ch, evaluating them, and sending the results back
              ;; on result-ch.  However, the user (or some background thread) might break
              ;; or kill the evaluation thread at any time, in which case this protocol could
              ;; fail.  We could try to enumerate and handle all such failure modes explicitly,
              ;; but a simple timeout, inelegant as it may be, lets us recover from all of them.
              (sync/timeout 1 (channel-put-evt in-user-ch thunk))
              (send (get-ints) run-in-evaluation-thread thunk)))
        
        ;; Returns whether the user thread is free, which is the case when:
        ;; - it's not dead, AND either
        ;;   * it's been suspended by the debugger, OR
        ;;   * it's done running the user's program.
        (define/private (user-thread-free?)
          (and (not (send (get-ints) evaluation-thread-dead?))
               (or (get-break-status) (not (is-running?)))))
        
        (define/public (render v)
          ;; ==drscheme eventspace thread==
          ;; returns false if the user thread is unavailable to perform the rendering
          (and (user-thread-free?)
          (let ([result-ch (make-channel)]
                [v (truncate-value v 100 5)])
            (do-in-user-thread
             (lambda ()
               (let ([s (open-output-string)])
                 (send (drscheme:language-configuration:language-settings-language 
                        current-language-settings)
                       render-value
                       v
                       (drscheme:language-configuration:language-settings-settings
                        current-language-settings)
                       s)
                      ;; Set a timeout in the user thread, so we don't block forever if the
                      ;; drscheme thread gives up waiting for our response.
                      (sync/timeout 1 (channel-put-evt result-ch (get-output-string s))))))
                 ;; Set a timeout to guard against cases where the user thread
                 ;; gets interrupted or killed in the middle of evaluation.
                 (sync/timeout 1 result-ch))))
        
        (define/public (print-to-console v)
          ;; ==drscheme eventspace thread==
          ;; only when a user thread is suspended
          (do-in-user-thread (lambda () (eprintf " ### DEBUGGER: ~s\n" v))))
        
        (define/private (frame->end-breakpoint-status frame)
          (let/ec k
            (let* ([stx (mark-source frame)]
                   [src (syntax-source stx)]
                   [pos (if (not (syntax-position stx))
                            (k 'invalid)
                            (+ (syntax-position stx) (syntax-span stx) -1))]
                   [defs (filename->defs src)]
                   [tab (if defs (send defs get-tab) (k 'invalid))]
                   [bps (send tab get-breakpoints)])
              (hash-ref bps pos 'invalid))))
        
        (define/private (can-step-over? frames status)
          (and (or (not (zero? (get-frame-num))) (eq? status 'entry-break))
               frames
               (not (empty? frames))
               (not (eq? (frame->end-breakpoint-status (list-ref frames (get-frame-num))) 'invalid))))
        
        (define/private (can-step-out? frames status)
          (and frames
            (let ([frames (list-tail frames (get-frame-num))])
              (and (not (empty? frames))
                   (ormap (lambda (f) (not (eq? (frame->end-breakpoint-status f) 'invalid)))
                          (rest frames))))))

        (define/public suspend-gui
          (lambda (frames status [switch-tabs? #f] [already-stopped? #f])
            (let ([top-of-stack? (zero? (get-frame-num))]
                  [status-message (send (get-frame) get-status-message)])
              (set! want-suspend-on-break? #f)
              (set-single-step?! #f)
              (set-box! stack-frames frames)
              (set-box! break-status status)
              (send (send (get-frame) get-pause-button) enable #f)
              (send (send (get-frame) get-step-button) enable top-of-stack?)
              (send (send (get-frame) get-step-over-button) enable (can-step-over? frames status))
              (send (send (get-frame) get-step-out-button) enable (can-step-out? frames status))
              (send (send (get-frame) get-resume-button) enable #t)
              (when (cons? frames)
                (send (get-frame) register-stack-frames frames already-stopped?)
                (send (get-frame) register-vars (list-ref frames (get-frame-num))))
              (send status-message set-label
                    (if (and (cons? status) top-of-stack?)
                        (let ([expr (mark-source (first frames))])
                          (cond
                            ; should succeed unless the user closes a slave tab during debugging
                            [(filename->defs (syntax-source expr))
                             => (lambda (defs)
                                  (clean-status
                                   (string-append
                                    (if (syntax-position expr)
                                        (trim-expr-str
                                         (send defs get-text
                                               (sub1 (syntax-position expr))
                                               (+ -1 (syntax-position expr) (syntax-span expr))))
                                        "??")
                                    " => "
                                    (if (= 2 (length status))
                                        (render (cadr status))
                                        (string-append
                                         "(values"
                                         (let loop ([vals (rest status)])
                                           (cond
                                             [(cons? vals) (string-append " " (render (first vals))
                                                                          (loop (rest vals)))]
                                             [else ")"])))))))]
                            [""]))
                        ""))
              (cond [(get-current-frame-endpoints)
                     => (lambda (start/end)
                          (cond [(and (first start/end) (defs-containing-current-frame))
                                 => (lambda (defs)
                                      (cond
                                        [(and switch-tabs? (send defs get-filename))
                                         => (lambda (fn) (handler:edit-file fn))])
                                      (send defs scroll-to-position (first start/end)))]))])
              (send (get-defs) invalidate-bitmap-cache))))
        
        (define/public (resume-gui)
          (update-trace-editor)
          (resume-gui-no-log))
        
        (define/public (resume-gui-no-log)
          (set-box! stack-frames #f)
          (set-box! break-status #f)
          (set-box! frame-num 0)
          (send (send (get-frame) get-pause-button) enable #t)
          (send (send (get-frame) get-step-button) enable #f)
          (send (send (get-frame) get-step-over-button) enable #f)
          (send (send (get-frame) get-step-out-button) enable #f)
          (send (send (get-frame) get-resume-button) enable #f)
          (send (send (get-frame) get-status-message) set-label "")
          (send (get-frame) clear-stack-frames/vars)
          (send (get-defs) invalidate-bitmap-cache))
        
        (define/public (resume-handler)
          (let ([v (get-break-status)])
            (when (sync/timeout 1 (channel-put-evt resume-ch (and (pair? v) (cdr v))))
              (resume-gui-no-log))))
        
        (define/public suspend
          ;; ==called from user thread==
          (lambda (break-handler frames [status #f])
            ;; suspend-sema ensures that we allow only one suspended thread
            ;;  at a time
            (cond
              [(semaphore-try-wait? suspend-sema)
               (parameterize ([current-eventspace (send (get-frame) get-eventspace)])
                 (queue-callback (lambda ()
                                   (update-trace-editor)
                                   (suspend-gui frames status #t))))
               (with-handlers ([exn:break?
                                (lambda (exn)
                                  (let ([wait-sema (make-semaphore)])
                                    (parameterize ([current-eventspace (send (get-frame) get-eventspace)])
                                      (queue-callback (lambda ()
                                                        (resume-gui)
                                                        (semaphore-post wait-sema))))
                                    (semaphore-wait wait-sema))
                                  (semaphore-post suspend-sema)
                                  (break-handler exn))])
                 (begin0
                   (let loop ()
                     (sync/enable-break
                      resume-ch (handle-evt in-user-ch (lambda (thunk)
                                                         (thunk)
                                                         (loop)))))
                   (semaphore-post suspend-sema)))]
              [(pair? status) (cdr status)]
              [else #f])))
        
        (define/public (prepare-execution debug?)
          (set! want-debug? debug?)
          (cond
            [debug? (send (get-frame) show-debug)]
            [else (send (get-frame) hide-debug)
                  (set! master this)
                  (for-each (lambda (t) (send t prepare-execution #f)) slaves)
                  (set! slaves empty)])
          (set! current-language-settings (and debug? (send (get-defs) get-next-settings)))
          (set! single-step? (box #t))
          (set! pos-vec (make-vector (add1 (send (get-defs) last-position)) #f))
          (set! top-level-bindings empty)
          (set! resume-ch (make-channel))
          (set! suspend-sema (make-semaphore 1))
          (set! in-user-ch (make-channel))
          (set! break-status (box #f))
          (set! want-suspend-on-break? #f)
          (set! stack-frames (box #f))
          (set! outmost-cms empty)
          (set! log-pairs empty)
          (set! id-counts (make-hasheq))
          (send (get-ints) set-tab this))
        
        (define/augment (on-close)
          (inner (void) on-close)
          (set-box! closed? #t)
          (for-each (lambda (t) (send t prepare-execution #f)) slaves))
        
        (define/public (hide-debug)
          (send (get-frame) hide-debug))
        
        (super-new)))

    ;; define icons for the tool and debug panel
    (define debug-bitmap (compiled-bitmap (debugger-icon)))
    (define small-debug-bitmap (compiled-bitmap (small-debugger-icon)))
    (define over-bitmap
      (compiled-bitmap
        (pict->bitmap
          (cc-superimpose (bitmap (text-icon "()" #:color syntax-icon-color))
                          (bitmap (right-over-arrow-icon #:color run-icon-color))))))
    (define out-bitmap
      (compiled-bitmap
        (pict->bitmap
          (hc-append
            -8
            (bitmap (text-icon "()" #:color syntax-icon-color))
            (bitmap (right-arrow-icon #:color run-icon-color #:height 19))))))

    (define pause-bitmap (compiled-bitmap (pause-icon #:color run-icon-color)))
    (define resume-bitmap (compiled-bitmap (play-icon #:color run-icon-color)))
    (define step-bitmap (compiled-bitmap (step-icon #:color run-icon-color)))

    (define make-pause-label (bitmap-label-maker "Pause" pause-bitmap))
    (define make-resume-label (bitmap-label-maker "Go" resume-bitmap))
    (define make-step-label (bitmap-label-maker "Step" step-bitmap))
    (define make-over-label (bitmap-label-maker "Over" over-bitmap))
    (define make-out-label (bitmap-label-maker "Out" out-bitmap))
    
    (define highlight-color (make-object color% 207 255 207))
    (define bold-sd (make-object style-delta% 'change-weight 'bold))
    (define normal-sd (make-object style-delta% 'change-weight 'normal))
    
    (define (debug-unit-frame-mixin super%)
      (class super%
        
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text
                 get-menu-bar
                 get-current-tab
                 get-top-level-window
                 get-eventspace)
        
        (define current-language-settings #f)
        (define control-panel #f)
        (define debug? #f)
        (define/public (set-mouse-over-msg msg)
          (when (not (string=? msg (send mouse-over-message get-label)))
            (send mouse-over-message set-label msg)))
        
        (define/public (debug-callback)
          (let ([tab (get-current-tab)])
            (cond
              [(eq? tab (send tab get-master))
               (set! debug? #t)
               (execute-callback)
               (set! debug? #f)]
              [else
               (already-debugging tab)])))
        
        (define/override (execute-callback)
          (let ([tab (get-current-tab)])
            (cond
              [(eq? tab (send tab get-master))
               (send (get-current-tab) prepare-execution debug?)
               (super execute-callback)]
              [else
               (already-debugging tab)])))
        
        (define/private (already-debugging tab)
          (message-box
           "Debugger"
           (format "This file is involved in a debugging session.  To run/debug this file, finish the session for ~a and close or re-run it."
                   (send (send (send tab get-master) get-defs) get-filename/untitled-name))
           this '(ok)))
        
        (define expr-positions empty)
        (define expr-lengths empty)
        
        (define/public (register-vars frame)
          (send variables-text begin-edit-sequence)
          (send variables-text lock #f)
          (send variables-text delete 0 (send variables-text last-position))
          (for-each
           (lambda (name/value)
             (let ([name (format "~a" (syntax-e (first name/value)))]
                   [value (format " => ~s\n" (truncate-value (second name/value) 100 5))])
               (send variables-text insert name)
               (send variables-text change-style bold-sd
                     (- (send variables-text last-position) (string-length name))
                     (send variables-text last-position))
               (send variables-text insert value)
               (send variables-text change-style normal-sd
                     (- (send variables-text last-position) (string-length value))
                     (send variables-text last-position))))
           (third (expose-mark frame)))
          (send variables-text lock #t)
          (send variables-text end-edit-sequence))
        
        (define/public (register-stack-frames frames already-stopped?)
          (let* ([trimmed-exprs
                  (map (lambda (frame)
                         (let ([expr (mark-source frame)])
                           (cond
                             ; should succeed unless the user closes a slave tab during debugging
                             [(and expr (filename->defs (syntax-source expr)))
                              => (lambda (defs)
                                   (trim-expr-str
                                    (if (syntax-position expr)
                                        (send defs get-text
                                              (sub1 (syntax-position expr))
                                              (+ -1 (syntax-position expr) (syntax-span expr)))
                                        "??")
                                    15))]
                             ["??"])))
                       frames)]
                 [trimmed-lengths (map add1 (map string-length trimmed-exprs))]
                 [positions (foldl + 0 trimmed-lengths)])
            (send stack-frames begin-edit-sequence)
            (send stack-frames lock #f)
            (unless already-stopped?
              (send stack-frames delete 0 (send stack-frames last-position))
              (for-each (lambda (trimmed-expr)
                          (send stack-frames insert (format "~a\n" trimmed-expr)))
                        trimmed-exprs))
            (send stack-frames change-style normal-sd 0 (send stack-frames last-position))
            (send stack-frames change-style bold-sd
                  (send stack-frames paragraph-start-position (send (get-current-tab) get-frame-num))
                  (send stack-frames paragraph-end-position (send (get-current-tab) get-frame-num)))
            (send stack-frames lock #t)
            (send stack-frames end-edit-sequence)))
        
        (define/public (clear-stack-frames/vars)
          (send stack-frames begin-edit-sequence)
          (send stack-frames lock #f)
          (send stack-frames delete 0 (send stack-frames last-position))
          (send stack-frames lock #t)
          (send stack-frames end-edit-sequence)
          (send variables-text begin-edit-sequence)
          (send variables-text lock #f)
          (send variables-text delete 0 (send variables-text last-position))
          (send variables-text lock #t)
          (send variables-text end-edit-sequence))
        
        (define/public (clear-trace-editor)
          (send num-button enable #f)
          (send sort-button enable #f)
          (send call-editor begin-edit-sequence)
          (send call-editor lock #f)
          (send call-editor delete 0 (send call-editor last-position))
          (send call-editor lock #t)
          (send call-editor end-edit-sequence)          
          (send output-editor begin-edit-sequence)
          (send output-editor lock #f)
          (send output-editor delete 0 (send output-editor last-position))
          (send output-editor lock #t)
          (send output-editor end-edit-sequence)
          (send view-canvas set-clear-flag #t)
          (send view-canvas refresh))
        
        (define/private (update-panel)
          (if (zero? (send tab-panel get-selection))
              (send swapping-panel active-child stack-view-panel)
              (send swapping-panel active-child trace-view-panel)))
        
        (define debug-grandparent-panel 'uninitialized-debug-grandparent-panel)
        (define debug-parent-panel 'uninitialized-debug-parent-panel)
        (define debug-panel 'uninitialized-debug-panel)
        (define log-button-panel 'uninitialized-log-button-panel)
        (define tab-panel 'uninitialized-tab-panel)
        (define swapping-panel 'uninitialized-swapping-panel)
        (define stack-view-panel 'uninitialized-stack-view-panel)
        (define stack-frames 'uninitialized-stack-frames)
        (define variables-text 'uninitialized-variables-text)
        (define trace-view-panel 'uninitialized-trace-view-panel)
        (define call-editor 'uninitialized-call-editor)
        (define output-editor 'uninitialized-output-editor)
        (define view-canvas 'uninitialized-view-canvas)
        (define mouse-over-frame #f)
        (define/override (get-definitions/interactions-panel-parent)
          (set! debug-grandparent-panel
                (new (class panel:horizontal-dragable%
                       (inherit get-percentages)
                       (define/augment (get-default-percentages i)
                         (cond
                           [(= i 2) 
                            (define p (preferences:get 'plt:debug-tool:stack/variable-area))
                            (list p (- 1 p))]
                           [else (build-list i (λ (x) (/ i)))]))
                       (define/augment (after-percentage-change)
                         (define ps (get-percentages))
                         (when (= (length ps) 2)
                           (preferences:set 'plt:debug-tool:stack/variable-area (car ps)))
                         (inner (void) after-percentage-change))
                       (super-new))
                     [parent (super get-definitions/interactions-panel-parent)]))
          (set! tab-panel
                (new tab-panel%
                     [parent debug-grandparent-panel]
                     [callback (λ (x y) (update-panel))]
                     [choices (list "Stack View" "Trace View")]
                     [min-width 160]
                     [stretchable-width #f]))
          (set! swapping-panel 
                (new panel:single% 
                     [parent tab-panel]))
          (set! stack-view-panel
                (new panel:vertical-dragable%
                     [parent swapping-panel]))
          (set! stack-frames
                (new (class text%
                       (super-new)
                       (inherit find-line line-start-position line-end-position
                                change-style begin-edit-sequence end-edit-sequence
                                lock last-position line-paragraph find-position
                                dc-location-to-editor-location)
                       (define highlight-defs #f)
                       (define highlight-start #f)
                       (define highlight-end #f)
                       (define mouse-over-frame #f)
                       (define/override (on-event evt)
                         (let*-values ([(x y) (dc-location-to-editor-location
                                               (send evt get-x) (send evt get-y))]
                                       [(line) (find-line y)]
                                       [(pos) (find-position x y)]
                                       [(paragraph) (line-paragraph line)]
                                       [(frames) (send (get-current-tab) get-stack-frames)]
                                       [(frame) (and frames
                                                     (> (length frames) paragraph)
                                                     (list-ref frames paragraph))]
                                       [(expr) (and frame (mark-source frame))])
                           (case (send evt get-event-type)
                             [(enter motion)
                              (when (and mouse-over-frame (not (= paragraph mouse-over-frame)))
                                ;; motion to different frame: unhighlight old
                                (send highlight-defs unhighlight-range
                                      highlight-start highlight-end highlight-color)
                                (set! mouse-over-frame #f))
                              (when (and expr (not (eq? mouse-over-frame paragraph)))
                                ;; motion to frame: highlight new
                                (cond
                                  [(filename->defs (syntax-source expr))
                                   => (lambda (defs)
                                        (set! mouse-over-frame paragraph)
                                        (set! highlight-defs defs)
                                        (set! highlight-start (sub1 (syntax-position expr)))
                                        (set! highlight-end (+ -1 (syntax-position expr)
                                                               (syntax-span expr)))
                                        (send defs highlight-range
                                              highlight-start highlight-end highlight-color)
                                        (cond
                                          [(send defs get-filename)
                                           => (lambda (fn) (handler:edit-file fn))])
                                        (send defs scroll-to-position (syntax-position expr)))]))]
                             [(leave)
                              (when mouse-over-frame
                                ;; motion to different frame: unhighlight old
                                (send highlight-defs unhighlight-range
                                      highlight-start highlight-end highlight-color)
                                (set! mouse-over-frame #f))
                              (cond
                                [(send (get-current-tab) get-frame-num)
                                 => (lambda (num) (send (get-current-tab) move-to-frame num))])]
                             [(left-down)
                              (when (and paragraph expr)
                                (send (get-current-tab) move-to-frame paragraph))]
                             [else (void)]))))))
          (set! variables-text (new text% [auto-wrap #f]))
          (let ([stack-frames-panel (make-object vertical-panel% stack-view-panel)])
            (new message% [parent stack-frames-panel] [label "Stack"])
            (new editor-canvas% [parent stack-frames-panel] [editor stack-frames] [style '(auto-hscroll)]))
          (let ([variables-panel (make-object vertical-panel% stack-view-panel)])
            (new message% [parent variables-panel] [label "Variables"])
            (new editor-canvas% [parent variables-panel] [editor variables-text] [style '(auto-hscroll)]))
          
          ;; traces view panel
          (set! trace-view-panel
                (new panel:vertical-dragable%
                     [parent swapping-panel]))
          ;; xiangqi li
          (set! call-editor
                (new (class text%
                       (inherit begin-edit-sequence
                                end-edit-sequence
                                lock
                                delete
                                insert
                                last-position
                                dc-location-to-editor-location
                                find-line
                                find-position
                                line-paragraph
                                change-style
                                paragraph-start-position
                                paragraph-end-position
                                scroll-to-position)
                       
                       (define call-procs empty)
                       (define mark-num 0)
                       
                       (define/public (display-call-proc procs mark? stopped?)
                         (begin-edit-sequence)
                         (lock #f)
                         (set! call-procs procs)
                         (unless mark?
                           (set! mark-num (sub1 (length procs))))
                         (unless stopped?
                           (delete 0 (last-position))
                           (for-each (lambda (proc) (insert (format "~a\n" proc))) procs))
                         (change-style normal-sd 0 (last-position))
                         (change-style bold-sd
                                       (paragraph-start-position mark-num)
                                       (paragraph-end-position mark-num))
                         (lock #t)
                         (end-edit-sequence))
                       
                       (define/public (move-to-frame the-frame-num)
                         (set! mark-num the-frame-num) 
                         (display-call-proc call-procs #t #t))                       
                       
                       (define/override (on-event evt)
                         (let*-values ([(x y) (dc-location-to-editor-location (send evt get-x) (send evt get-y))]
                                       [(line) (find-line y)]
                                       [(paragraph) (line-paragraph line)])
                           (case (send evt get-event-type)
                             [(left-down)
                              (when (< paragraph (length call-procs))
                                (move-to-frame paragraph)
                                (send num-button enable #f)
                                (send output-editor set-call-num paragraph)
                                (send view-canvas set-clear-flag #t)
                                (send view-canvas refresh))])))                       
                       
                       
                       (super-new))))
          ;; xiangqi li
          (set! output-editor
                (new (class text%
         
                       (inherit begin-edit-sequence
                                end-edit-sequence
                                lock
                                delete
                                insert
                                last-position
                                dc-location-to-editor-location
                                find-line
                                find-position
                                line-paragraph
                                change-style
                                paragraph-start-position
                                paragraph-end-position
                                scroll-to-position)
         
                       (super-new)
         
                       (define var-stack-expressions #f)
                       (define var-cm #f)
                       (define highlight-defs #f)
                       (define highlight-start #f)
                       (define highlight-end #f)
                       (define mouse-over-var #f)
                       (define mark-nums empty)
                       (define trace-logs empty)
                       
                       (define/public (get-trace-logs) trace-logs)

                       (define/public (display-logs logs mark? stopped?)
                         (begin-edit-sequence)
                         (lock #f)
                         (unless stopped?
                           (delete 0 (last-position))
                           (for-each (lambda (log) (insert log)) logs))
                         (when mark?
                           (change-style normal-sd 0 (last-position))
                           (for ([n mark-nums])
                             (change-style bold-sd
                                           (paragraph-start-position n)
                                           (paragraph-end-position n))))
                         (lock #t)
                         (end-edit-sequence))
                       
                       (define/public (sort-logs)
                         (set! var-cm (sort var-cm (lambda (x y)
                                                     (string<? (format "~a" (syntax->datum (car x)))
                                                               (format "~a" (syntax->datum (car y)))))))
                         (set! trace-logs (map (lambda (i) (format "~a: ~v\n" (syntax->datum (first i)) (second i))) var-cm))
                         (display-logs trace-logs #f #f))
                       
                       (define/public (get-selected-var/size)
                         (let*-values ([(var) (first (list-ref var-cm (car mark-nums)))]
                                       [(target other) (partition (lambda (c)
                                                                    (eq? (first c) var))
                                                                  var-cm)])
                           (values (syntax->datum var) (length target))))
                       
                       (define/public (trim-trace-size n size)
                         (when n
                           (if (> (string->number n) size)
                               (message-box "Wrong input" "Number exceeds maximum size!" #f '(ok caution))
                               (void))))
                                    
                             ;(printf "target: ~a, other: ~a\n" target other))))
                           ;(printf "var: ~a\n" (syntax->datum (first (list-ref var-cm (car mark-nums)))))))

                       (define/public (move-to-frame the-frame-num)
                         (set! mark-nums (list the-frame-num))
                         (display-logs trace-logs #t #t))
         
                       (define/public (set-var-stack-exprs l)
                         (set! var-stack-expressions l)
                         (set-call-num (sub1 (length l))))
                       
                       (define/public (set-call-num n)
                         (set! var-cm (list-ref var-stack-expressions n))
                         (set! trace-logs (map (lambda (i) (format "~a: ~v\n" (syntax->datum (first i)) (second i))) var-cm))
                         (display-logs trace-logs #f #f))
                       
                       (define/public (mark-trace-with-same-continuations frames var-num frame-num exprs)
                         (let* ([source (take (list-ref frames var-num) frame-num)]
                                [src-length (length source)]
                                [same? #t])
                           (set! mark-nums empty)
                           (for ([j (in-range (length frames))])
                             (when (<= src-length (length (list-ref frames j)))
                               (set! same? #t)
                               (for ([i (in-range (length source))])
                                 (unless (eq? (list-ref (list-ref frames j) i) (list-ref source i))
                                   (set! same? #f)))
                               (when same?
                                 (send num-button enable #f)
                                 (set! mark-nums (append mark-nums (list j))))))
                           (display-logs trace-logs #t #t)))
         
                       (define/override (on-event evt)
                         (let*-values ([(x y) (dc-location-to-editor-location (send evt get-x) (send evt get-y))]
                                       [(line) (find-line y)]
                                       [(paragraph) (line-paragraph line)]
                                       [(var) (and var-cm
                                                   (> (length var-cm) paragraph)
                                                   (first (list-ref var-cm paragraph)))])
                           (case (send evt get-event-type)
                             [(enter motion)
                              (when (and mouse-over-var (not (eq? paragraph mouse-over-var)))
                                (send highlight-defs unhighlight-range
                                      highlight-start highlight-end highlight-color)
                                (set! mouse-over-var #f))
                              (when (and var (not (eq? paragraph mouse-over-var)))
                                (cond 
                                  [(filename->defs (syntax-source var))
                                   => (lambda (defs)
                                        (set! mouse-over-var paragraph)
                                        (set! highlight-defs defs)
                                        (set! highlight-start (sub1 (syntax-position var)))
                                        (set! highlight-end (+ -1 (syntax-position var) (syntax-span var)))
                                        (send defs highlight-range
                                              highlight-start highlight-end highlight-color)
                                        (cond
                                          [(send defs get-filename)
                                           => (lambda (fn) (handler:edit-file fn))])
                                        (send defs scroll-to-position (syntax-position var)))]))]
                             [(leave)
                              (when mouse-over-var
                                (send highlight-defs unhighlight-range
                                      highlight-start highlight-end highlight-color)
                                (set! mouse-over-var #f))]
                             [(left-down)
                              (when (< paragraph (length var-cm))
                                (move-to-frame paragraph)
                                (send num-button enable #t)
                                (send view-canvas update-stack (map third var-cm) (map fourth var-cm))
                                (send view-canvas set-var-num paragraph))])))
                    
                       )))
          ;; xiangqi li
          (define env-canvas%
            (class canvas%
    
              (inherit get-dc
                       refresh)
    
              (super-new)
    
              (define var-num 0)
              (define stack-size 0)
              (define stack-exprs empty)
              (define stack-expr empty)
              (define stack-frames #f)
              (define width 0)
              (define start-x 5)
              (define start-y 50)
              (define rec-height 20)
              (define text-x (+ start-x 4))
              (define render-call-stack void)
              (define mouse-over-stack-frame #f)
              (define highlight-defs #f)
              (define highlight-start #f)
              (define highlight-end #f)
              (define clear-flag #f)
    
              (define/private (get-text-width str)
                (first (call-with-values (lambda () (send (get-dc) get-text-extent str)) list)))
    
              (define/public (get-max-text-width l)
                (apply max (map (lambda (x) (get-text-width x)) l)))
              
              (define/public (update-stack exprs frames)
                (set! stack-exprs exprs)
                (set! stack-frames frames))
              
              (define/public (set-clear-flag b) (set! clear-flag b))
              
              (define/public (set-var-num n)
                (set! var-num n)
                (set! stack-expr (list-ref stack-exprs var-num))
                (set! stack-size (length stack-expr))
                (set! width (+ (get-max-text-width stack-expr) 20))
                (set! render-call-stack (lambda () (render-call-stack-unit 0)))
                (set-clear-flag #f)
                (refresh))
              
              (define/private (render-call-stack-unit i)
                (let ([current-y (+ start-y (* i rec-height))])
                  (when (< i stack-size)
                    (send (get-dc) draw-rectangle start-x current-y width rec-height)
                    (send (get-dc) draw-text (list-ref stack-expr i) text-x current-y)
                    (render-call-stack-unit (add1 i)))))
        
              (define/private (mouse-over-frame? x y start-x width start-y frame-size)
                (define (x-in-range? x) (and (>= x start-x)
                                             (<= x (+ start-x width))))
                (define (y-in-range? y i)
                  (if (and (<= (+ start-y (* i rec-height)) y)
                           (<= y (+ start-y (* i rec-height) rec-height))
                           (< i frame-size))
                      (values #t i)
                      (if (>= i frame-size)
                          (values #f i)
                          (y-in-range? y (+ i 1)))))
                (if (x-in-range? x)
                    (y-in-range? y 0)
                    (values #f frame-size)))
        
              (define/override (on-event evt)
                (let*-values ([(event-x) (send evt get-x)]
                              [(event-y) (send evt get-y)]
                              [(over-stack? stack-frame-num)
                               (mouse-over-frame? event-x event-y start-x width start-y stack-size)]
                              [(frame) (and over-stack?
                                            (> stack-size stack-frame-num)
                                            (list-ref (list-ref stack-frames var-num) stack-frame-num))]
                              [(expr) (and frame (mark-source frame))])
                  (case (send evt get-event-type)
                    [(enter motion)
                     (when (and mouse-over-stack-frame (not (eq? stack-frame-num mouse-over-stack-frame)))
                       (send highlight-defs unhighlight-range
                             highlight-start highlight-end highlight-color)
                       (set! mouse-over-stack-frame #f))
                     (when (and expr (not (eq? mouse-over-stack-frame stack-frame-num)))
                       (cond
                         [(filename->defs (syntax-source expr))
                          => (lambda (defs)
                               (set! mouse-over-stack-frame stack-frame-num)
                               (set! highlight-defs defs)
                               (set! highlight-start (sub1 (syntax-position expr)))
                               (set! highlight-end (+ -1 (syntax-position expr)
                                                      (syntax-span expr)))
                               (send defs highlight-range
                                     highlight-start highlight-end highlight-color)
                               (cond
                                 [(send defs get-filename)
                                  => (lambda (fn) (handler:edit-file fn))])
                               (send defs scroll-to-position (syntax-position expr)))]))]
                    [(leave)
                     (when mouse-over-stack-frame
                       (send highlight-defs unhighlight-range
                             highlight-start highlight-end highlight-color)
                       (set! mouse-over-stack-frame #f))]
                    [(left-down)
                     (when (and stack-frame-num expr)
                       (send output-editor move-to-frame stack-frame-num)
                       (send output-editor mark-trace-with-same-continuations
                             (map reverse stack-frames)
                             var-num
                             (- stack-size stack-frame-num)
                             (map reverse stack-exprs)))]
              
                    )))    
      
              (define/override (on-paint)
                (if clear-flag
                    (send (get-dc) clear)
                    (render-call-stack)))

              ))
          
          (let ([ver-panel (make-object vertical-panel% trace-view-panel)])
            (set! log-button-panel (instantiate horizontal-panel% ()
                                     (parent ver-panel)
                                     (stretchable-height #f)
                                     (alignment '(right center))
                                     (style '(border))))
            (new message% [parent log-button-panel] [label "Log"] [stretchable-width #t])
            (let ([log-panel (new panel:horizontal-dragable% [parent ver-panel])])
              (new editor-canvas% [parent log-panel] [editor call-editor] [style '(auto-hscroll)])
              (new editor-canvas% [parent log-panel] [editor output-editor] [style '(auto-hscroll)])))
          (let ([env-view-panel (make-object vertical-panel% trace-view-panel)])
            (new message% [parent env-view-panel] [label "Visualization"])
            (set! view-canvas (new env-canvas% [parent env-view-panel])))
          
          ;; parent of panel with debug buttons
          (set! debug-parent-panel
                (make-object vertical-panel% debug-grandparent-panel))
          ;; horizontal panel with debug buttons; not vertically stretchable
          (set! debug-panel (instantiate horizontal-panel% ()
                              (parent debug-parent-panel)
                              (stretchable-height #f)
                              (alignment '(center center))
                              (style '(border))))
          ;; add a close button to the debug panel
          (new close-icon%
               [parent debug-panel]
               [callback (λ () (send (get-current-tab) break-callback)
                               (hide-debug))])
          ;; hide the debug panel and stack view initially
          (send debug-parent-panel change-children (lambda (l) null))
          (send debug-grandparent-panel change-children (lambda (l) (remq tab-panel l)))
          (make-object vertical-panel% debug-parent-panel))

        (define/public (get-output-editor) output-editor)
        (define/public (get-view-canvas) view-canvas)
        (define/public (get-call-editor) call-editor)
        
        (define/public (hide-debug)
          (when (member debug-panel (send debug-parent-panel get-children))
            (send debug-grandparent-panel change-children
                  (lambda (l) (remq tab-panel l)))
            (send debug-parent-panel change-children
                  (lambda (l) (remq debug-panel l)))))
        
        (define/public (show-debug)
          (unless (member debug-panel (send debug-parent-panel get-children))
            (send debug-grandparent-panel change-children
                  (lambda (l) (append l (list tab-panel))))
            (send debug-parent-panel change-children
                  (lambda (l) (cons debug-panel l)))))
        
        (super-new)
        
        (define status-message
          (instantiate message% ()
            [label " "]
            [parent debug-panel]
            [stretchable-width #t]))
        
        (define debug-button
          (new switchable-button%
               (label (string-constant debug-tool-button-name))
               (bitmap debug-bitmap)
               (alternate-bitmap small-debug-bitmap)
               (parent (new panel:horizontal-discrete-sizes%
                            [parent (get-button-panel)]
                            [alignment '(center center)]))
               (callback (λ (button) (debug-callback)))))
        (inherit register-toolbar-button)
        (register-toolbar-button debug-button #:number 60)

        (define/augment (enable-evaluation)
          (send debug-button enable #t)
          (inner (void) enable-evaluation))

        (define/augment (disable-evaluation)
          (send debug-button enable #f)
          (inner (void) disable-evaluation))
        
        (define num-button
          (instantiate button% ()
            [label "num"]
            [parent log-button-panel]
            [callback (lambda (button evt)
                        (let-values ([(var size) (send output-editor get-selected-var/size)])
                          (send output-editor trim-trace-size
                                (get-text-from-user (format "Set size of ~a traces (< ~a)" var size) #f) size)))]
            [enabled #f]))
        
        (define sort-button
          (instantiate button% ()
            [label "sort"]
            [parent log-button-panel]
            [callback (lambda (button evt)
                        (let ([traces (send output-editor get-trace-logs)])
                          (unless (empty? traces)
                            (send output-editor sort-logs))))]
            [enabled #f]))        

        (define pause-button
          (instantiate button% ()
            [label (make-pause-label this)]
            [parent debug-panel]
            [callback (lambda (button evt)
                        (cond [(send (get-current-tab) get-stack-frames) (bell)]
                              [else (send (get-current-tab) suspend-on-break? #t)
                                    (send (get-current-tab) break-callback)
                                    (send (get-current-tab) reset-offer-kill)]))]
            [enabled #t]))
        
        (define resume-button
          (instantiate button% ()
            [label (make-resume-label this)]
            [parent debug-panel]
            [callback (lambda (button evt)
                        (if (send (get-current-tab) get-stack-frames)
                            (send (get-current-tab) resume-handler)
                            (bell)))]
            [enabled #f]))
        
        (define step-button
          (instantiate button% ()
            [label (make-step-label this)]
            [parent debug-panel]
            [callback (lambda (btn evt)
                        (cond [(send (get-current-tab) get-stack-frames)
                               (send (get-current-tab) set-single-step?! #t)
                               (send (get-current-tab) resume-handler)]
                              [else (bell)]))]
            [enabled #f]))
       
        (define (make-big-step-callback out?)
          (lambda (btn evt)
            ; go through stack frames until it's possible to set a breakpoint at the end
            (let* ([frames (list-tail (send (get-current-tab) get-stack-frames)
                                      (send (get-current-tab) get-frame-num))]
                   [frames (case (send (get-current-tab) get-break-status)
                             [(entry-break) (if out? (rest frames) frames)]
                             [else (if out? (rest frames) empty)])]
                   [frame (ormap (lambda (f depth)
                                   (let/ec k
                                     (let* ([stx (mark-source f)]
                                            [src (syntax-source stx)]
                                            [lpos (or (syntax-position stx) (k #f))]
                                            [pos (+ lpos (syntax-span stx) -1)]
                                            [defs (filename->defs src)]
                                            [tab (if defs (send defs get-tab) (k #f))]
                                            [bps (send tab get-breakpoints)]
                                            [cur-stat (hash-ref bps pos 'invalid)])
                                       (case cur-stat
                                         [(invalid) #f]
                                         [else
                                          (hash-set!
                                           bps pos
                                           (lambda ()
                                             (and (< (length (continuation-mark-set->list
                                                              (current-continuation-marks) debug-key)) depth)
                                                  (begin
                                                    (hash-set! bps pos cur-stat)
                                                    #t))))
                                          f]))))
                                 frames
                                 (let ([len (length frames)])
                                   (build-list len (lambda (i) (- len i)))))])
              (cond [frames (send (get-current-tab) set-single-step?! (not frame))
                            (send (get-current-tab) resume-handler)]
                    [else (bell)]))))
        
        (define step-over-button
          (new button%
               [label (make-over-label this)]
               [parent debug-panel]
               [callback (make-big-step-callback #f)]
               [enabled #f]))
        
        (define step-out-button
          (new button%
               [label (make-out-label this)]
               [parent debug-panel]
               [callback (make-big-step-callback #t)]
               [enabled #f]))
        
        (define/public (get-debug-button) debug-button)
        (define/public (get-pause-button) pause-button)
        (define/public (get-resume-button) resume-button)
        (define/public (get-step-button) step-button)
        (define/public (get-step-over-button) step-over-button)
        (define/public (get-step-out-button) step-out-button)
        (define/public (get-status-message) status-message)
        (define/public (get-sort-button) sort-button)
        (define/public (get-num-button) num-button)
        
        (define mouse-over-message
          (instantiate message% ()
            [label " "] [parent debug-panel] [stretchable-width #t]))
        
        (define/augment (on-tab-change old new)
          (check-current-language-for-debugger)
          (if (send new debug?)
              (let ([status (send new get-break-status)])
                (if status
                    (begin
                      (send new update-trace-editor)
                      (send new suspend-gui (send new get-stack-frames) status #f #t))
                    (send new resume-gui))
                (show-debug))
              (hide-debug))
          (inner (void) on-tab-change old new))
        
        (define/public (check-current-language-for-debugger)
          (let* ([settings (send (get-definitions-text) get-next-settings)]
                 [lang (drscheme:language-configuration:language-settings-language settings)]
                 [visible? (and (send lang capability-value 'gui-debugger:debug-button)
                                (not (is-a? lang drscheme:module-language:module-language<%>)) ;; the opt-out button handles this language
                                (not (debugger-does-not-work-for?
                                      (extract-language-level settings))))])
            (if visible?
                (unless (send debug-button is-shown?)
                  (send (send debug-button get-parent) add-child debug-button))
                (when (send debug-button is-shown?)
                  (send (send debug-button get-parent) delete-child debug-button)))))
        
        (send (get-button-panel) change-children
              (lambda (children)
                (cons (send debug-button get-parent)
                      (remq (send debug-button get-parent) children))))
        
        ; hide debug button if it's not supported for the initial language:
        (check-current-language-for-debugger)))
    
    (drscheme:language:register-capability 'gui-debugger:debug-button (flat-contract boolean?) #t)
    (drscheme:get/extend:extend-definitions-text debug-definitions-text-mixin)
    (drscheme:get/extend:extend-interactions-text debug-interactions-text-mixin)
    (drscheme:get/extend:extend-unit-frame debug-unit-frame-mixin)
    (drscheme:get/extend:extend-tab debug-tab-mixin)))
