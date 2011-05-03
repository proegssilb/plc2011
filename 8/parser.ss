(load "chez-init.ss")

(define varlist?
  (lambda (x)
    (cond [(null? x) #f]
	  [(atom? x) #f]
	  [else (and (symbol? (car x)) (or (null? (cdr x)) (varlist? (cdr x))))]
	  )
    ))

(define mk-exp-list
  (lambda (expr)
    (if (null? expr)
	(null-exp-node)
	(exp-list-node (parse-expression (car expr)) (mk-exp-list (cdr expr)))
    )
    ))

(define mk-var-list
  (lambda (vars)
    (cond [(null? vars) (null-var-node)]
	  [(symbol? vars) (improp-node vars)]
	  [(symbol? (car vars)) (var-list-node (car vars) (mk-var-list (cdr vars)))]
	  [else (eopl:error 'parse-expression "Invalid entry in var list ~s" vars)])
    ))

(define var-list->list
  (lambda (vars)
    (cases var-list vars
	   (null-var-node () '())
	   (improp-node (var) var)
	   (var-list-node (var next) (cons var (var-list->list next))))
    ))

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

(define exp-list->list
  (lambda (exprs)
    (cases exp-list exprs
	   (null-exp-node () '())
	   (exp-list-node (exp nxt) 
			  (cons (unparse-expression exp) (exp-list->list nxt)))
	   )))

(define-datatype expression expression? 
  (null-exp )
  (var-exp
   (id symbol?))
  (lit-exp
   (val (lambda (x) #t)))
  (lambda-exp
   (id symbol?)
   (body exp-list?))
  (lambda-exp-args
   (ids var-list?)
   (body exp-list?))
  (if-exp 
   (condition expression?)
   (body expression?))
  (if-else-exp
   (condition expression?)
   (t-case expression?)
   (f-case expression?))
  (let-exp
   (type symbol?)
   (defs defs-list?)
   (body exp-list?))
  (named-let-exp
   (name symbol?)
   (defs defs-list?)
   (body exp-list?))
  (set-exp
   (var symbol?)
   (val expression?))
  (app-exp
   (rator expression?)
   (rand exp-list?))
  (begin-exp
   (exps exp-list?))
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
	    [(member (car datum) '(let let* letrec) )
	     (cond [(or (< (length datum) 2) (null? (cadr datum)))
		    (eopl:error 'parse-expression "Can't have null-definition let. ~s" datum)]
		   [(or (< (length datum) 3) (null? (caddr datum)))
		    (eopl:error 'parse-expression "null-body let not allowed. ~s" datum)]
		   [(and (> (length datum) 3) (symbol? (cadr datum)) (pair? (caddr datum)))
		    (named-let-exp (cadr datum) (mk-defs-list (caddr datum)) (mk-exp-list (cdddr datum)))]
		   [else (let-exp (car datum) (mk-defs-list (cadr datum)) (mk-exp-list (cddr datum)))]
		   )]
	    [(eqv? (car datum) 'begin) (begin-exp (mk-exp-list (cdr datum)))]
	    [(eqv? (car datum) 'set!)
	     (if (not (= (length datum) 3)) (eopl:error 'parse-expression 
							"Incorrect number of items for set! ~s" datum))
	     (set-exp (cadr datum) (parse-expression (caddr datum)))]
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