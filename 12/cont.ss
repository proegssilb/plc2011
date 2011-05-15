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
	   [if-cont (true-exp false-exp next-cont env)
		    (if val
			(eval-expression true-exp next-cont env)
			(eval-expression false-exp next-cont env))])))
