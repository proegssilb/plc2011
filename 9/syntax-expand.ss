;case, cond, and, or
(define syntax-expand
  (lambda (exp)
    (cases expression exp
	  [let-exp (syms exps bodies) (let->application syms exps bodies)]
	  [let*-exp (params values bodies) (let*->application params values bodies)]
	  [cond-exp (test then) (cond->if test then)]
	  [and-exp (tests) exp]
	  [or-exp (tests) exp]
	  [case-exp (key lists exps) (case->if key lists exps)]
	  [else exp]
	  )))
	    

(define let->application
  (lambda (vars exps bodies)
    (app-exp (lambda-exp-args (mk-var-list vars) bodies) exps)
    ))

;(define let*->let
;  (lambda (params values bodies)
;    (letrec [(helper 
;               (lambda (params values bodies)
;                 (if (= (length params) 1)
;		           (let-exp (list (car params)) (list (car values)) bodies)
;				   (let-exp (list (car params)) (list (car values)) (list (helper (cdr params) (cdr values) bodies)))
;				   )
;		 ))]
;      (helper params values bodies))))

(define let*->application
  (lambda (params values bodies)
    (letrec [(helper
	            (lambda (params values bodies)
				  (if (= (length params) 1)
				    (app-exp (lambda-exp-args (list (car params)) bodies) (list (car values)))
					(app-exp (lambda-exp-args (list (car params)) (list (helper (cdr params) (cdr values) bodies))) (list (car values)))
				  )))]
	(helper params values bodies))))



(define cond->if
  (lambda (tests then)
    (letrec [(helper
	           (lambda (tests then)
			     
			     (cond
				   [(equal? (car tests) (var-exp 'else)) (car then)]
				   [(= (length tests) 1) (if-exp (car tests) (car then))]
				   [else (if-else-exp (car tests) (car then) (helper (cdr tests) (cdr then)))]
				 )
			   ))]
	(helper tests then))))



(define case->if
  (lambda (key lists exps)
    (letrec [(helper (lambda (arg ls)
		       (if (or (null? ls) (null? (car ls))) 
			   '()
			   (cond [(eq? (caar ls) 'else) (cadar ls)]
				 [(list? (caar ls)) 
				  (append 
				   (list 'if (list 'contains? (caar ls) arg) (cadar ls)) 
				   (list (helper arg (cdr ls)))
				   )]
				 [else 
				  (append 
				   (list 'if (list 'eq? arg (caar ls)) (cadar ls))
				   (list (helper arg (cdr ls)))
				   )]))
		       ))]
      (helper (cadr lists) (cddr lists))
      )))

(define contains?
  (lambda (x y)
    (if(null? x) #f
       (if(equal? y (car x)) #t
          (contains? (cdr x) y)))))