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
    
    (inherit get-area-container)
    
    (super-new (label (make-label))
               (width 800)
               (height 600))
    
    (define widget (new widget% 
                        [parent (get-area-container)]))
    
    (define/public (get-widget) widget)
         
    (define/private (make-label)
      (if filename
          (string-append (path->string
                          (file-name-from-path filename))
                         " - Trace browser")
          "Trace browser"))))


