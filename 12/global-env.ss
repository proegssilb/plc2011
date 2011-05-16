(load "chez-init.ss")

(define global-environment '())

(define reset-global-env
  (case-lambda 
   [() (reset-global-env init-env-syms init-env-vals)]
   [(syms vals)
    (if (or (null? syms) (null? vals))
	(set! global-environment '())
	(let ([curr-sym (car syms)]
	      [curr-val (car vals)]
	      [nxt-syms (cdr syms)]
	      [nxt-vals (cdr vals)])
	  (reset-global-env nxt-syms nxt-vals)
	  (set! global-environment (cons (cons curr-sym curr-val) global-environment))
	  ))]
    ))

(define extend-global-env
    (lambda (sym val)
      (let ([curr (assoc sym global-environment)])
	(if curr
	    (set! global-environment (cons 
				      (cons sym val) 
				      (remove curr global-environment)
				      ))
	    (set! global-environment (cons (cons sym val) global-environment))
	    ))
	  ))

(define global-set-var
  (lambda (sym val)
    (let ([curr (assoc sym global-environment)])
      (if (not curr)
	  (eopl:error 'global-set-var "Var doesn't exist ~s" sym)
	  (set! global-environment (cons 
				    (cons sym val) 
				    (remove curr global-environment)
				    ))
	  ))
    ))

(define apply-global-env
  (lambda (sym)
    (let ([curr (assoc sym global-environment)])
      (if curr
	  (cdr curr)
	  (eopl:error 'apply-global-env "Var doesn't exist ~s" sym)
	  ))
    ))

(define init-env-syms
'( +
   -
   *
   /
   add1
   sub1
   zero?
   =
   <
   >
   <=
   >=
   cons
   car
   cdr
   list
   null?
   eq?
   equal?
   atom?
   length
   list->vector
   list?
   pair?
   procedure?
   vector->list
   vector
   make-vector
   vector-ref
   vector?
   number?
   symbol?
   set-car!
   set-cdr!
   vector-set!
   caar
   cadr
   cdar
   cddr
   caaar
   caadr
   cadar
   caddr
   cdaar
   cdadr
   cddar
   cdddr
   map
   apply
   assq
   assv
   append
   set-car!
   ))

(define init-env-vals
  (list
   +
   -
   *
   /
   (lambda (x) (+ x 1))
   (lambda (x) (- x 1))
   zero?
   =
   <
   >
   <=
   >=
   cons
   car
   cdr
   list
   null?
   eq?
   equal?
   atom?
   length
   list->vector
   list?
   pair?
   procedure?
   vector->list
   vector
   make-vector
   vector-ref
   vector?
   number?
   symbol?
   set-car!
   set-cdr!
   vector-set!
   caar
   cadr
   cdar
   cddr
   caaar
   caadr
   cadar
   caddr
   cdaar
   cdadr
   cddar
   cdddr
   map
   apply
   assq
   assv
   append
   set-car!
   ))