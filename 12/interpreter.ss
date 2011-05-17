;;; David Bliss
;;; Interpreting parse trees routines

(load "cont.ss")

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
		  [if-exp (condition body) ;(eval-if condition body env)]
			  (let ([ifcnt (if-cont body '() cont env)])
			    (apply-cont (eval-cont condition ifcnt env)))]
		  [if-else-exp (condition t-case f-case)
			       (let ([ifcnt (if-cont t-case f-case cont env)])
				 (apply-cont (eval-cont condition ifcnt env)))]
		  [let-exp (syms exps bodies) '()]
		  [let*-exp (params values bodies) '()]
		  [letrec-exp (proc-names params values bodies) '()]
		  [named-let-exp (name defs body) '()]
		  [set-exp (var val) '()]
		  [begin-exp (exps)
			    (apply-cont (begin-cont exps cont env) '())]
		  [app-exp (operator operand)
			   (eval-tree operator 
				      env 
				      (eval-cont operand (proc-cont cont) env)
				      )]
		  [and-exp (tests)
			   (apply-cont (and-exp tests cont env) #t)]
		  [or-exp (tests)
			  (apply-cont (or-exp tests cont env) #f)]
		  [while-exp (test exps)
			     (apply-cont (while-test-cont test exps cont env) #f)]
		  [call-cc-exp (func)
			       (eval-tree func env (call-cc-cont cont env))] 
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
     [(and (symbol? id) (atom? body)) 
      (closure-record id body env)]
     [(and (symbol? id) (list? body)) 
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
   (id symbol?)
   (body expression?)
   (env list?)]
  [multiclosure-record
   (ids (list-of symbol?))
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