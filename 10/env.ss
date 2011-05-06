(load "chez-init.ss")
(load "parser.ss")

;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define-datatype env-ref-entry env-ref?
  (env-reference
   (sym symbol?)
   (targ-env list?))
  )

(define empty-env
    (lambda ()
          '()))

(define transform-refs-syms
  (lambda (syms vals ref-env)
    (cond [(null? syms) '()]
	  [(null? vals) '()]
	  [(reference? (car syms))
	   (let ([sym (refTarget (car syms))]
		 [val (env-reference (car vals) ref-env)])
	     (cons sym (transform-refs (cdr syms) (cdr vals) ref-env)))]
	  [else 
	   (let ([sym (car syms)]
		 [val (car vals)])
	     (cons sym (transform-refs-syms (cdr syms) (cdr vals) ref-env)))])
    ))

(define transform-refs-vals
  (lambda (syms vals ref-env)
    (cond [(null? syms) '()]
	  [(null? vals) '()]
	  [(reference? (car syms))
	   (let ([sym (refTarget (car syms))]
		 [val (env-reference (car vals) ref-env)])
	     (cons val (transform-refs (cdr syms) (cdr vals) ref-env)))]
	  [else 
	   (let ([sym (car syms)]
		 [val (car vals)])
	     (cons sym (transform-refs-vals (cdr syms) (cdr vals) ref-env)))])
    ))

(define refTarget
  (lambda (r)
    (cond [(reference? r) (cases reference r
				   (ref (targ) targ))]
	  [(env-ref? r) (cases env-ref-entry r
			       (env-reference (targ env) targ))]
	  )))

(define extend-env
  (lambda (syms vals env)
    (let ([vals (list->vector vals)])
      (cons (cons 
	     (transform-refs-syms syms (vector->list vals) env)
	     (list->vector (transform-refs-vals syms (vector->list vals) env)))
	    env))
    ))

;(define extend-env
;  (lambda (syms vals env)
;    (cons (cons syms (list->vector vals)) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (car (car env))]
	      [vals (cdr (car env))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		(let ([val (vector-ref vals pos)])
		  (if (env-ref? val)
		      (cases env-ref-entry val
			     (env-reference (sym ref-env)
					    (apply-env ref-env sym)))
		      val))
		(apply-env env sym)))))))

(define update-env
  (lambda (env sym val)
    (if (null? env)
	#f
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		(let ([val (vector-ref vals pos)])
		  (if (env-ref? val)
		      (cases env-ref-entry val
			     (env-reference (sym ref-env)
					    (update-env ref-env sym val)))
		      (begin (vector-set! vals pos val) #t)))
		(update-env env sym val)
		))
	  ))
    ))

(define find-position
  (lambda (sym ls)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) 0]
	  [else (let ([index (find-position sym (cdr ls))])
		  (if (number? index)
		      (+ index 1)
		      #f))])))
(define init-env
  (lambda ()
    (extend-env init-env-syms
		init-env-vals
		(empty-env))
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
   max
   list
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
   max
   list
   ))
