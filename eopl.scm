(define duple
  (lambda (n obj)
    (if (<= n 0)
	'()
	(cons obj (duple (- n 1) obj)))))
(duple 2 3)
(define path
  (lambda (n bst)
    (if (< n (car bst))
	(cons 'left (path n (cadr bst)))
	(if (eq? n (car bst))
	    '()
	    (cons 'right (path n (caddr bst)))))))
(path 17 '(14 (7 () (12 () ()))
	      (26 (20 ( 17 () ())
		      ())
		  (31 () ()))))
(define exer136
  (lambda (x lst)
    (cons x
	  (if (null? lst) '()
	      (exer136 (cons (+ 1 (caar lst)) (cdar lst))
		       (cdr lst))))))
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
	(exer136 (list 0 (car lst)) (number-elements (cdr lst))))))
(number-elements '(1 2 3 4))
					; Exercise 2.20
(define number->bintree
  (lambda (n)
    (list 'left (list n '() '()) '())))
(define current-element
  (lambda (bintree)
    (if (null? bintree) (error "No element in tree")
	(caadr bintree))))
(define left-subtree
  (lambda (bintree)
    (if (null? bintree) (error "No left subtree")
	(cadadr bintree))))
(define right-subtree
  (lambda (bintree)
    (if (null? bintree) (error "No right subtree")
	(car (cddadr bintree)))))
(define move-to-left-son
  (lambda (bintree)
    (if (null? (left-subtree bintree)) (error "No left child")
    (list 'left
	  (left-subtree bintree)
	  (cons
	   (list (car bintree)
		 (current-element bintree)
		 (right-subtree bintree))
	   (caddr bintree))))))
(define move-to-right-son
  (lambda (bintree)
    (if (null? (right-subtree bintree)) (error "No right child")
    (list 'right
	  (right-subtree bintree)
	  (cons
	   (list (car bintree)
		 (current-element bintree)
		 (left-subtree bintree))
	   (caddr bintree))))))
(define at-leaf?
  (lambda (bintree)
    (and (null? (left-subtree bintree))
	 (null? (right-subtree bintree)))))
(define insert-to-left
  (lambda (n bintree)
    (list (car bintree)
	  (list (current-element bintree)
	        (list n (left-subtree bintree) '())
	        (right-subtree bintree))
	  (caddr bintree))))
(define insert-to-right
  (lambda (n bintree)
    (list (car bintree)
	  (list (current-element bintree)
		(left-subtree bintree)
		(list n '() (right-subtree bintree)))
	  (caddr bintree))))
(define upper-tree
  (lambda (bintree)
    (caaddr bintree)))
(define move-up
  (lambda (bintree)
    (list (car (upper-tree bintree))
	  (cons (cadr (upper-tree bintree))
		(if (eq? (car bintree) 'left)
		    (list (cadr bintree)
			  (caddr (upper-tree bintree)))
		    (list (caddr (upper-tree bintree))
			  (cadr bintree)))))))
(define at-root?
  (lambda (bintree)
    (null? (caddr bintree))))
(define-syntax define-variant
  (syntax-rules ()
    ((define-variant variant (field-name ...) (field-value ...))
     (lambda (some-name)
       (cond
	((eq? some-name 'variant) #t)
	((eq? some-name 'field-name) 'field-value) ...
	(else #f))))))
(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype name name? (variant (field field?) ...) ...)
     (begin
      (define and:define-datatype (lambda (x y) (and x y)))
      (define name?
	(lambda (data)
	  (cond
	   ((data 'variant)
	    (reduce and:define-datatype #t (list (field? (data 'field)) ...))) ... 
	   (else #f))))
      (define variant
	(lambda (field ...)
	  (lambda (some-name)
	    (cond
	     ((eqv? some-name 'variant) #t)
	     ((eqv? some-name 'field) field) ...
	     ((eqv? some-name 'name) (list field ...))
	     (else #f))))) ...))))
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define identity (lambda-exp 'x (var-exp 'x)))
(define-syntax cases
  (syntax-rules (else)
    ((cases name exp
	    (variant (field ...) consequent) ...)
     (cond ((exp 'variant)
	    (apply (lambda (field ...) consequent) (exp 'name))) ...))
    ((cases name exp clauses (else default))
     ((cases name exp clauses) (else default)))))
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
      
      
	  
