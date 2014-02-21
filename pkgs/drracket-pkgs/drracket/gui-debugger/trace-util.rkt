#lang racket

(provide (all-defined-out))
(struct traced-value (val trace) #:inspector #f)
(struct atree (ftree ptree) #:inspector #f)
(struct dtree (label node rtree) #:inspector #f)

;; to-track? : (listof traced-value) -> boolean
;;  retains the previous traces when there's a traced application
(define (to-track? lst)
  (if (null? lst)
      #f
      (let ([fst (first lst)])
        (if (traced-value? fst)
            (if (eq? (dtree-label (traced-value-trace fst)) 'app)
                #t
                (to-track? (rest lst)))
            (to-track? (rest lst))))))

;; passes in function name and traced version of arguments
(define (ap fun exp . args)
  (cond 
    [(traced-value? fun)
     (let* ([res (apply (traced-value-val fun) args)]
            [context (cons (continuation-mark-set-first (current-continuation-marks) 'stack null) exp)] 
            [new-ftree (attach-stack-info (traced-value-trace fun) context)]
            [traced-res (traced-value res (dtree 'app (atree new-ftree
                                                             (map (lambda (a) (if (traced-value? a) (traced-value-trace a) a)) args))
                                                 (dtree 'lf res #f)))])
       (if (traced-value? res)
           (traced-value (traced-value-val res)
                         (dtree 'app (atree new-ftree (map (lambda (a) (if (traced-value? a) (traced-value-trace a) a)) args)) (traced-value-trace res)))
           (traced-res)))]
    [else
     (let* ([params (map (lambda (a) (if (traced-value? a) (traced-value-val a) a)) args)]
            [raw-res (apply fun params)]
            [res (if (traced-value? raw-res) (traced-value-val raw-res) raw-res)]
            [context (append (continuation-mark-set-first #f 'inspect null) (list exp))])
       (if (traced-value? raw-res)
           (traced-value res (dtree 'app (atree (dtree 'lfl (cons fun context) #f) (map (lambda (a) (when (traced-value? a) (traced-value-trace a))) args)) (traced-value-trace raw-res)))
           (if (to-track? args)
               (traced-value res (dtree 'app (atree (dtree 'lfl (cons fun context) #f) (map (lambda (a) (when (traced-value? a) (traced-value-trace a))) args))
                                        (dtree 'lf res #f)))
               (traced-value res (dtree 'lf res #f)))))]))

(define (attach-stack-info tree context)
  (dtree (dtree-label tree) (cons (dtree-node tree) context) (dtree-rtree tree)))