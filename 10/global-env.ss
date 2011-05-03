(load "chez-init.ss")

(define global-environment '())

(define reset-global-env
  (lambda ()
    (set! global-environment '())
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
	  curr
	  (eopl:error 'apply-global-env "Var doesn't exist ~s" sym)
	  ))
    ))