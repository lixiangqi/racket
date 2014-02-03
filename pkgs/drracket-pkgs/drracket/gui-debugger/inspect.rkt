#lang racket

(provide inspect)
 
#;(define-syntax (inspect stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc stx (inspect id #:num 100 #:label ""))]
    [(_ id #:num n) (syntax/loc stx (inspect id #:num n #:label ""))]
    [(_ id #:label l) (syntax/loc stx (inspect id #:num 100 #:label l))]
    [(_ id #:num n #:label l) (identifier? #'id) 
                              (syntax-property (syntax/loc stx (void id)) 'inspect (list 'id (syntax-e #'n) (syntax-e #'l)))]
    [(_ id #:label l #:num n) (identifier? #'id)
                              (syntax/loc stx (inspect id #:num n #:label l))]
    [(_ exp #:num n #:label l) 
     (syntax-case #'exp ()
       [(f . args) (syntax-property (syntax/loc stx (void exp)) 'inspect (list 'app (syntax-e #'n) (syntax-e #'l)))])]
    [(_ exp #:label l #:num n) (syntax/loc stx (inspect exp #:num n #:label l))]))

(define-syntax (inspect stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc stx (inspect id #:num 100 #:label ""))]
    [(_ id #:num n) (syntax/loc stx (inspect id #:num n #:label ""))]
    [(_ id #:label l) (syntax/loc stx (inspect id #:num 100 #:label l))]
    [(_ id #:num n #:label l) 
                              (syntax-property (syntax/loc stx (void id)) 'inspect (list 'id (syntax-e #'n) (syntax-e #'l)))]
    [(_ id #:label l #:num n) 
                              (syntax/loc stx (inspect id #:num n #:label l))]))
    