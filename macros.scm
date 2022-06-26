(use-modules (srfi srfi-1)) ;; for guile
(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype name name? (variant (field field?) ...) ...)
     (begin
      (define name?
	(lambda (data)
	  (cond
	   ((data 'variant)
	    (reduce (lambda (x y) (and x y))
		    #t
		    (list (field? (data 'field)) ...))) ... 
	   (else #f))))
      (define variant
	(lambda (field ...)
	  (lambda (some-name)
	    (cond
	     ((eqv? some-name 'variant) #t)
	     ((eqv? some-name 'field) field) ...
	     ((eqv? some-name 'name) (list field ...))
	     (else #f))))) ...))))
       
(define-syntax cases
  (syntax-rules (else expand)
    ((cases name exp (v (f ...) c) . rest)
     ;; start to expand
     (cases expand name exp ((v (f ...) c)) . rest))
    ((cases expand name exp ((v (f ...) c) ...) (variant (field ...) consequent) . rest)
     ;; more to expand
     (cases expand name exp ((v (f ...) c) ... (variant (field ...) consequent)) . rest))
    ((cases expand name exp ((v (f ...) c) ...) (else default))
     ;; finished expanding, if there is an else clause
     (cond ((exp 'v) (apply (lambda (f ...) c) (exp 'name))) ... (else default))) 
    ((cases expand name exp
	    ((variant (field ...) consequent) ...))
     ;; finished expanding, if there was no else clause
     (cond ((exp 'variant)
	    (apply (lambda (field ...) consequent) (exp 'name))) ...))))
        