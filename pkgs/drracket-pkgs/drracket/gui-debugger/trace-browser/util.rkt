#lang racket/base
(require racket/class)
(provide with-unlock)

(define-syntax with-unlock
  (syntax-rules ()
    [(with-unlock text . body)
     (let* ([t text]
            [locked? (send t is-locked?)])
       (dynamic-wind
         (lambda ()
           (send* t
             (begin-edit-sequence #f)
             (lock #f)))
         (lambda () . body)
         (lambda ()
           (send* t
             (lock locked?)
             (end-edit-sequence)))))]))