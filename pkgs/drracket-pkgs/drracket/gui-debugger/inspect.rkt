#lang racket

(provide inspect)
 
(define-syntax (inspect stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc stx (inspect id #:num 50))]
    [(_ id #:num n) (identifier? #'id) (syntax-property (syntax/loc stx (void id)) 'inspect (syntax-e #'n))]))

