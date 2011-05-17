(load "chez-init.ss")
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define varlist?
  (lambda (x)
    (cond [(null? x) #f]
	  [(atom? x) #f]
	  [else (and (symbol? (car x)) (or (null? (cdr x)) (varlist? (cdr x))))]
	  )
    ))

;(define mk-exp-list
;  (lambda (expr)
;    (if (null? expr)
;	(null-exp-node)
;	(exp-list-node (parse-expression (car expr)) (mk-exp-list (cdr expr)))
;    )
;    ))

(define mk-exp-list
  (lambda (expr)
    (map parse-expression expr)
    ))

;(define mk-var-list
;  (lambda (vars)
;    (cond [(null? vars) (null-var-node)]
;	  [(symbol? vars) (improp-node vars)]
;	  [(symbol? (car vars)) (var-list-node (car vars) (mk-var-list (cdr vars)))]
;	  [else (eopl:error 'parse-expression "Invalid entry in var list ~s" vars)])
;    ))

(define mk-var-list
  (lambda (vars)
    (map (lambda (v) 
	   (if (symbol? v) 
	       v
	       (eopl:error 'parse-expression "Invalid entry in var list ~s" v)))
	 vars)
    ))

;(define var-list->list
;  (lambda (vars)
;    (cases var-list vars
;	   (null-var-node () '())
;	   (improp-node (var) var)
;	   (var-list-node (var next) (cons var (var-list->list next))))
;    ))
(define (var-list->list vars) vars)

(define mk-defs-list
  (lambda (defs)
    (cond [(null? defs) (null-def-node)]
	  [else (defs-list-node (caar defs) 
		  (parse-expression (cadar defs)) 
		  (mk-defs-list (cdr defs))
		  )])
    ))

(define defs-list->list
  (lambda (defs)
    (cases defs-list defs
	   (null-def-node () '())
	   (defs-list-node (var def next)
	     (cons (list var (unparse-expression def))
		     (defs-list->list next)))
	   )))

;(define exp-list->list
;  (lambda (exprs)
;    (cases exp-list exprs
;	   (null-exp-node () '())
;	   (exp-list-node (exp nxt) 
;			  (cons (unparse-expression exp) (exp-list->list nxt)))
;	   )))

(define exp-list->list
  (lambda (exprs)
    (map unparse-expression exprs)
    ))

(define-datatype expression expression? 
  (null-exp )
  (var-exp
   (id symbol?))
  (lit-exp
   (val (lambda (x) #t)))
  (lambda-exp
   (id symbol?)
   (body (list-of expression?)))
  (lambda-exp-args
   (ids (list-of symbol?))
   (body (list-of expression?)))
  (if-exp 
   (condition expression?)
   (body expression?))
  (if-else-exp
   (condition expression?)
   (t-case expression?)
   (f-case expression?))
  (let-exp
   (syms (list-of symbol?))
   (exps (list-of expression?))
   (bodies (list-of expression?)))
  (let*-exp
   (params (list-of symbol?))
   (values (list-of expression?))
   (bodies (list-of expression?)))
  (letrec-exp
    (proc-names (list-of symbol?))
    (params (list-of (list-of symbol?)))
    (values (list-of (list-of expression?)))
    (bodies (list-of expression?)))
  (named-let-exp
   (name symbol?)
   (params (list-of symbol?))
   (values (list-of expression?))
   (bodies (list-of expression?)))
  (set-exp
   (var symbol?)
   (val expression?))
  (app-exp
   (rator expression?)
   (rand (list-of expression?)))
  (begin-exp
   (exps (list-of expression?)))
  (cond-exp
   (test (list-of expression?))
   (then (list-of expression?)))
  (and-exp
   (tests (list-of expression?)))
  (or-exp
   (tests (list-of expression?)))
  (case-exp
   (key expression?)
   (lists (list-of (list-of expression?)))
   (exps (list-of expression?)))
  (while-exp
   (test expression?)
   (bodies (list-of expression?)))
  (define-exp
    (param symbol?)
	(value expression?))
  (call-cc-exp
   (func expression?))
  )

(define-datatype var-list var-list?
  (null-var-node )
  (improp-node
   (var symbol?))
  (var-list-node
   (var symbol?)
   (next var-list?))
  )

(define-datatype exp-list exp-list?
  (null-exp-node )
  (exp-list-node
   (node-val expression?)
   (next exp-list?))
  )

(define-datatype defs-list defs-list?
  (null-def-node )
  (defs-list-node
    (var symbol?)
    (def expression?)
    (next defs-list?))
  )
  
  (define parse-list
  (lambda (ls)
    (if (null? ls)
        '()
        (if (pair? ls)
            (append (list (parse-expression (car ls))) (parse-list (cdr ls)))
            (list (parse-expression ls))))))

(define parse-single
  (lambda (ls)
    (parse-expression (car ls))))

(define good-let?
  (lambda (ls)
    (cond 
      ((null? ls) #t)
      ((not (list? ls)) #f)
      ((not (list? (car ls))) #f)
      ((and (symbol? (caar ls)) (not (null? (cdar ls))) (null? (cddar ls)))
             (good-let? (cdr ls)))
      (else #f))))

(define make-let-vars
  (lambda (ls)
    (if (null? ls)
        '()
    (cons (caar ls) (make-let-vars (cdr ls))))))

(define make-let-vals
  (lambda (ls)
    (if (null? ls)
        '()
          (cons (parse-expression (cadar ls)) (make-let-vals (cdr ls))))))
    
(define make-letrec-vals
  (lambda (ls)
    (if (null? ls)
        '()
        (if (pair? (cadar ls))
              (if (eqv? (caadar ls) 'lambda)
                  (cons (parse-list (cddr (cadar ls))) (make-letrec-vals (cdr ls)))
                  (cons (parse-expression (cadar ls)) (make-letrec-vals (cdr ls))))
              (cons (parse-expression (cadar ls)) (make-letrec-vals (cdr ls)))))))

(define parse-expression
  (lambda (datum)
    (cond
     [(null? datum) (null-exp)]
     [(symbol? datum) (var-exp datum)]
     [(atom? datum) (lit-exp datum)]
     [(pair? datum)
      (cond [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
	    [(eqv? (car datum) 'lambda)
	     (if (< (length datum) 3) (eopl:error 'parse-expression "Invalid lambda syntax ~s" datum))
	     (cond [(symbol? (cadr datum))
		    (lambda-exp (cadr datum)
				(mk-exp-list (cddr datum)))]
		   [(or (pair? (cadr datum)) (null? (cadr datum)))
		    (lambda-exp-args (mk-var-list (cadr datum))
				     (mk-exp-list (cddr datum)))]
		   
		   [else (eopl:error 'parse-expression 
				     "Invalid arglist to lambda ~s" datum)]
		   )]
	    [(eqv? (car datum) 'if)
	     (cond [(null? (cadr datum)) (eopl:error 'parse-expression
						     "Condition for if cannot be null")]
		   [(= (length datum) 3) 
		    (if-exp (parse-expression (cadr datum))
			    (parse-expression (caddr datum)))]
		   [(= (length datum) 4) 
		    (if-else-exp (parse-expression (cadr datum)) 
				 (parse-expression (caddr datum))
				 (parse-expression (cadddr datum)))]
		   [else (eopl:error 'parse-expression
				     "Invalid number of statements for if: ~s" datum)])
	     ]
		 
		 [(eqv? (car datum) 'cond)
		   (syntax-expand (cond-exp (map parse-expression (make-let-vars (cdr datum))) (make-let-vals (cdr datum))))]
		 [(eqv? (car datum) 'and)
		   (syntax-expand (and-exp (map parse-expression (cdr datum))))]
		 [(eqv? (car datum) 'or)
		   (syntax-expand (or-exp (map parse-expression (cdr datum))))]
		 [(eqv? (car datum) 'case)
		  (if (or (null? (cdr datum)) (null? (cddr datum)))
		      (eopl:error 'parse-expression "Incorrect case structure in ~s" datum)
		      (syntax-expand (case-exp (parse-expression (cadr datum)) (map 
										(lambda (x) 
										  (if (list? x) 
										      (map parse-expression x)
										      (list (parse-expression x))))
										(make-let-vars (cddr datum)))
					       (make-let-vals (cddr datum))
					       )))]
	    ((eqv? (car datum) 'let)
          (if (or (null? (cdr datum)) (null? (cddr datum)))
              (eopl:error 'parse-expression "Invalid let structure ~s" datum)
              (if (symbol? (cadr datum))
                    (if (null? (cdddr datum))
                        (eopl:error 'parse-expression "Invalid let structure ~s" datum)
                        (if (good-let? (caddr datum))
                            (syntax-expand (named-let-exp (cadr datum) (make-let-vars (caddr datum)) 
                              (make-let-vals (caddr datum)) (parse-list (cdddr datum))))
                            (eopl:error 'parse-expression "Bad named-let expression ~s" datum)))
                    (if (good-let? (cadr datum))
                        (syntax-expand (let-exp (make-let-vars (cadr datum)) (make-let-vals (cadr datum)) (parse-list (cddr datum))))
                        (eopl:error 'parse-expression "Bad let expression ~s" datum)))))
         ((eqv? (car datum) 'let*)
          (if (or (null? (cdr datum)) (null? (cddr datum)))
              (eopl:error 'parse-expression "Invalid let* structure ~s" datum)
              (if (good-let? (cadr datum))
                  (syntax-expand (let*-exp (make-let-vars (cadr datum)) (make-let-vals (cadr datum)) (parse-list (cddr datum))))
                  (eopl:error 'parse-expression "Invalid let* structure ~s" datum))))
         ((eqv? (car datum) 'letrec)
          (if (or (null? (cdr datum)) (null? (cddr datum)))
              (eopl:error 'parse-expression "Invalid letrec structure ~s" datum)
              (if (good-let? (cadr datum))
                  (let ((bodies (make-letrec-vals (cadr datum))))
                    (letrec-exp (make-let-vars (cadr datum)) (map cadadr (cadr datum)) bodies (parse-list (cddr datum))))
                  (eopl:error 'parse-expression "Bad letrec expression ~s" datum))))
				  
				  
	    [(eqv? (car datum) 'begin) (begin-exp (mk-exp-list (cdr datum)))]
	    [(eqv? (car datum) 'while) (while-exp (parse-expression (cadr datum)) (mk-exp-list (cddr datum)))]
	    [(eqv? (car datum) 'call/cc) (call-cc-exp (parse-expression (cadr datum)))]
	    [(eqv? (car datum) 'set!)
	     (if (not (= (length datum) 3)) (eopl:error 'parse-expression 
							"Incorrect number of items for set! ~s" datum))
	     (set-exp (cadr datum) (parse-expression (caddr datum)))]\
		[(eqv? (car datum) 'define) (define-exp (cadr datum) (parse-expression (caddr datum)))]
	    [else (app-exp (parse-expression (car datum)) (mk-exp-list (cdr datum)))]
	    )]
		   
      [else (eopl:error 'parse-expression
		       "Invalid concrete syntax ~s" datum)])
    ))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   [null-exp () '()]
	   [var-exp (id) id]
	   [lit-exp (val) val]
	   [lambda-exp (id body) 
		       (append (list 'lambda id)
			     (exp-list->list body))]
	   [lambda-exp-args (ids body)
			    (append (list 'lambda (var-list->list ids)) (exp-list->list body))]
	   [if-exp (condition body)
		   (list 'if (unparse-expression condition)
			 (unparse-expression body))]
	   [if-else-exp (condition t-case f-case)
			(list 'if (unparse-expression condition)
			      (unparse-expression t-case)
			      (unparse-expression f-case))]
	   [let-exp (type defs body) (append (list type) (list (defs-list->list defs)) (exp-list->list body))]
	   [named-let-exp (name defs body) (append '(let) 
						   (list name) 
						   (list (defs-list->list defs)) 
						   (exp-list->list body))]
	   [set-exp (var val) (list 'set! var (unparse-expression val))]
	   [app-exp (rator rand)
		    (cons (unparse-expression rator)
			  (exp-list->list rand))])
    ))

(define occurs-free?
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))