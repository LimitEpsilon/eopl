(load "./macros.scm")
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define identity (lambda-exp 'y (var-exp 'y)))
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
	   (var-exp (var) (eqv? var search-var))
	   (lambda-exp (bound-var body)
		       (and
			(not (eqv? search-var bound-var))
			(occurs-free? search-var body)))
	   (app-exp (rator rand)
		    (or
		     (occurs-free? search-var rator)
		     (occurs-free? search-var rand))))))
(occurs-free? 'x identity)
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))
(define s-list-test (non-empty-s-list (symbol-s-exp 'x) (empty-s-list)))
(s-list? s-list-test)
