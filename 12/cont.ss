(define-datatype continuation continuation?
  (halt-cont)
  (eval-cont
   (exps (list-of expression?))
   (cont continuation?)
   (env scheme-value?))
  (cons-cont
   (value scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (and-cont
   (exps (list-of expression?))
   (cont continuation?)
   (env pair?))
  (or-cont
   (exps (list-of expression?))
   (cont continuation?)
   (env pair?))
  (begin-cont
   (exps (list-of expression?))
   (cont continuation?)
   (env pair?))
  (while-body-cont
   (test expression?)
   (bodies (list-of expression?))
   (cont continuation?)
   (env pair?))
  (while-test-cont
   (test expression?)
   (bodies (list-of expression?))
   (cont continuation?)
   (env pair?))
  (call-cc-cont
   (cont continuation?) ;;; The procedure to call is passed in via val.
   (env pair?))
  (if-cont
   (true-exp expression?)
   (false-exp expression?)
   (next-cont continuation?)
   (env pair?)))


(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()
		      (pretty-print val)]
	   [eval-cont (exps cont env)
		      (eval-expressions exps (cons-cont val cont) env)]
           [proc-cont (cont)
		      (apply-procedure (car val) (cdr val) cont)]
	   [cons-cont (value cont)
		      (apply-cont cont (cons value val))]
	   [and-cont (exps next-cont env)
		     (if (or (null? exps) (not val))
			 (apply-continuation next-cont val)
			 (apply-continuation (eval-cont
					      (car exps)
					      (and-cont (cdr exps) next-cont env)
					      env)))]
	   [or-cont (exps next-cont env)
		     (if (or (null? exps) val)
			 (apply-continuation next-cont val)
			 (apply-continuation (eval-cont
					      (car exps)
					      (and-cont (cdr exps) next-cont env)
					      env)))]
	   [begin-cont (exps next-cont env)
		       (if (null? exps)
			   (apply-cont next-cont val)
			   (eval-tree (car exps) 
				      env
				      (begin-cont (cdr exps) next-cont env))
			   )]
	   [while-body-cont (test exps next-cont env)
			    (if val
				(eval-cont exps 
					   (while-test-cont test exps next-cont env)
					   env)
				(apply-cont next-cont val))]
	   [while-test-cont (test exps next-cont env)
			    (eval-tree test env (while-body-cont test exps next-cont env))]
	   [call-cc-cont (cont-to-pass env)
			 (apply-proc val cont-to-pass]
	   [if-cont (true-exp false-exp next-cont env)
		    (if val
			(eval-expression true-exp next-cont env)
			(eval-expression false-exp next-cont env))])))

