#lang racket

(provide (all-defined-out))
(struct traced-value (val trace) #:inspector #f)
(struct atree (ftree ptree rtree) #:inspector #f)
(struct dtree (label node) #:inspector #f)

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
(define (ap fun . args)
  (cond 
    [(traced-value? fun)
     (let* ([res (apply (traced-value-val fun) args)]
            [traced-res (traced-value res (dtree 'app (atree (traced-value-trace fun) 
                                                             (map (lambda (a) (when (traced-value? a) (traced-value-trace a))) args)
                                                             (dtree 'lf res))))])
       (if (traced-value? res)
           (traced-value (traced-value-val res)
                         (dtree 'app (atree (traced-value-trace fun) (map traced-value-trace args) (traced-value-trace res))))
           (traced-res)))]
    [else
     (let* ([params (map (lambda (a) (if (traced-value? a) (traced-value-val a) a)) args)]
            [res (apply fun params)])
       (printf "\n%%continuation=~a\n" (continuation-mark-set->list (current-continuation-marks) 'inspect))
       (printf "%%fun=~a, args=~a\n\n" fun args)
       (if (to-track? args)
           (traced-value res (dtree 'app (atree (dtree 'lff fun) (map (lambda (a) (when (traced-value? a) (traced-value-trace a))) args)
                                                (dtree 'lf res))))
           (traced-value res (dtree 'lf res))))]))

(define (append-function-trace v t)
  (let* ([val (traced-value-val v)]
         [node (dtree-node (traced-value-trace v))]
         [ftree (atree-ftree node)])
    (traced-value val
                  (dtree 'app (atree (dtree 'lff (cons (dtree-node ftree) t))
                                     (atree-ptree node)
                                     (atree-rtree node))))))
  