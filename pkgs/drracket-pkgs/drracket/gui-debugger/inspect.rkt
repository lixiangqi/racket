#lang racket

(provide inspect)
 
#;(define-syntax (inspect stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc stx (inspect id #:num 100))]
    [(_ id #:num n) (identifier? #'id) (syntax-property (syntax/loc stx (void id)) 'inspect (list 'id (syntax-e #'n)))]
    [(_ id #:num n) (syntax-property (syntax/loc stx (void id)) 'inspect (list 'app (syntax-e #'n)))]))

(define-syntax (inspect stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc stx (inspect id #:num 100 #:label ""))]
    [(_ id #:num n) (syntax/loc stx (inspect id #:num n #:label ""))]
    [(_ id #:label l) (syntax/loc stx (inspect id #:num 100 #:label l))]
    [(_ id #:num n #:label l) (identifier? #'id) 
                              (syntax-property (syntax/loc stx (void id)) 'inspect (list 'id (syntax-e #'n) (syntax-e #'l)))]
    [(_ id #:label l #:num n) (identifier? #'id)
                              (syntax/loc stx (inspect id #:num n #:label l))]
    [(_ (f . args) #:num n #:label l) (syntax-property (syntax/loc stx (void (f . args))) 'inspect (list 'app (syntax-e #'n) (syntax-e #'l)))]
    [(_ (f . args) #:label l #:num n) (syntax/loc stx (inspect (f . args) #:num n #:label l))])) 
