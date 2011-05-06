;;; David Bliss
;;; Interpreting parse trees routines

(define interpret
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   [initial-environment (init-env)]
	   [result (eval-tree parse-tree initial-environment)])
      result)))

(define eval-one-exp interpret)

(define eval-tree
  (lambda (exp env)
    (cond [(expression? exp)
	   (cases expression exp
		  [null-exp () '()]
		  [var-exp (id) (apply-env env id)]
		  [lit-exp (val) val]
		  [lambda-exp (id body)
			      (make-closure id body env)]
		  [lambda-exp-args (ids body)
				   (make-closure ids body env)]
		  [if-exp (condition body) (eval-if condition body env)]
		  [if-else-exp (condition t-case f-case)
			       (eval-if-else condition t-case f-case env)]
		  [let-exp (syms exps bodies) '()]
		  [let*-exp (params values bodies) '()]
		  ;[letrec-exp (proc-names params values bodies) '()]
		  ;[named-let-exp (name defs body) '()]
		  [set-exp (var val) (if (not (update-env env sym val))
					 (global-set-var var val))]
		  [begin-exp (exps)
			     (cases exp-list exps
				    [null-exp-node () '()]
				    [exp-list-node (val next) 
						   (cases exp-list next
							  [null-exp-node () (eval-tree val env)]
							  [exp-list-node (v2 n2)  (begin
										    (eval-tree val env) 
										    (eval-tree (begin-exp next) env))]
							  )])]
		  [app-exp (operator operand)
			   (let ([procedure (eval-tree operator env)]
				 [arg (eval-tree operand env)])
			     (apply-proc procedure arg env))]
		  [and-exp (tests)
			   (cond
			    [(null? tests) #t]
			    [(atom? tests) (eopl:error 'eval-tree "Parse error in and: ~s" tests)]
			    [(= (length tests) 1) (eval-tree (car tests) env)]
			    [(not (eval-tree (car tests) env)) #f]
			    [else (eval-tree (and-exp (cdr tests)) env)])]
		  [or-exp (tests)
			  (cond
			   [(null? tests) #f]
			   [(atom? tests) (eopl:error 'eval-tree "Parse error in or: ~s" tests)]
			   [(= (length tests) 1) (eval-tree (car tests) env)]
			   [else (let ([t1 (eval-tree (car tests) env)])
				   (if t1
				       t1
				       (eval-tree (or-exp (cdr tests)) env)
				       ))])]
		  [while-exp (test exps)
			     (letrec ([doBodies (lambda (exprs)
						  (if (null? exprs)
						      '()
						      (begin (eval-tree (car exprs) env) (doBodies (cdr exprs)))
						      ))]
				      [helper (lambda ()
						(if (eval-tree test env)
						    (begin (doBodies exps)
							   (helper)
						    )))])
			       (helper)
			       )]
		  [else (eopl:error 'eval-tree "Invalid case: ~s" exp)]
		  )]
	  ;[(exp-list? exp)
	   ;(cases exp-list exp
		  ;[null-exp-node () '()]
		  ;[exp-list-node (nv next) 
				 ;(cases exp-list next
					;[null-exp-node () (list (eval-tree nv env))]
					;[exp-list-node 
					 ;(nv2 next2) 
					 ;(cons (eval-tree nv env) (eval-tree next env))])
				 ;])]
	  [(list? exp) (map (lambda (x) (eval-tree x env)) exp)]
	  [else (eopl:error 'eval-tree "Invalid grammar: ~s" exp)])
    ))

;;; Closures
(define make-closure
  (lambda (id body env)
    (cond 
     [(and (variable? id) (atom? body)) 
      (closure-record id body env)]
     [(and (variable? id) (list? body)) 
      (multiclosure-record (mk-var-list id) body env)]
     [(and (list? id) (atom? body)) 
      (multiclosure-record id (mk-exp-list body) env)]
     [(and (list? id) (list? body)) 
      (multiclosure-record id body env)]
     [else (eopl:error 'make-closure "Could not make closure: ~s ~s ~s" id body env)]
     )
    ))

(define-datatype closure closure?
  [closure-record
   (id variable?)
   (body expression?)
   (env list?)]
  [multiclosure-record
   (ids (list-of variable?))
   (body (list-of expression?))
   (env list?)])

(define apply-proc
  (lambda (proc arg env)
    (if (closure? proc)
	(cases closure proc
	       [closure-record (id body env)
			       (car (reverse (eval-tree body (extend-env (list id) 
									 (list arg) 
									 env)
					  )))]
	       [multiclosure-record (ids body env)
				    (car (reverse (eval-tree body 
					       (extend-env 
						(var-list->list ids)
						arg
						env)
					       )))]
	       )
	(apply proc arg)
	;(proc arg)
	)))

;;;Utility functions for dealing with syntax
(define eval-if
  (lambda (condition body env)
    (if (eval-tree condition env) (eval-tree body env))
    ))

(define eval-if-else
  (lambda (condition t-case f-case env)
    (if (eval-tree condition env)
	(eval-tree t-case env)
	(eval-tree f-case env))
    ))