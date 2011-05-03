(load "env.ss")
(load "parser.ss")
(load "interpreter.ss")
(load "syntax-expand.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([foo (read)])
      (if (not (equal? foo '(exit)))
	  (begin (write (interpret foo))
		 (newline)
		 (rep))))))

(define-syntax for
  (syntax-rules (:)
    [(_ (e1 : e2 : e3) b1) 
     (begin 
	e1 
	(letrec [(helper (lambda () 
			   (if e2 (begin b1 e3 (helper)))
			   ))]
	  (helper)))]
    ))

(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...) (let [(a e1)] (begin e2 ...) a)]
    ))
