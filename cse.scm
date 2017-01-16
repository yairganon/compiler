(define gen-code 
	(lambda (exprs hash-table)
 		(cond ((null? hash-table)              exprs)
			  ((equal? 1 (length hash-table)) `(let ,hash-table ,exprs))
		 (else                                `(let* ,hash-table ,exprs)))))
				  
(define in-acc? 
	(lambda (x l)
		(and (not (null? l)) (member x (map car l)))))
		
(define extract-symbol 
	(lambda (x symbols)
		(let ((maybe-sym (filter (lambda (sym) (equal? (car (cdr sym)) x)) symbols)))
			(if (null? maybe-sym) x (caar maybe-sym)))))
	
(define count-occurrences
	(lambda (x lst . start)
		(fold-left (lambda (acc curr) 
		(cond 
			((equal? x curr) (+ 1 acc))
			((not (list? curr)) acc)
			((ormap list? curr) (count-occurrences x curr acc))
			(else acc)
			)) 
		(if (null? start) 0 (car start)) lst)))

(define only-if-bigger-then-one
	(lambda (curr expr)
		(let ((num-of-occ (count-occurrences curr expr)))
			(if (> num-of-occ 1) (list (cons curr num-of-occ)) '()))))
		
(define count-repets 
	(lambda (expr curr-expr hash)
		(fold-left 
			(lambda (acc curr)
				(cond 
					((not (list? curr)) acc)
					((equal? (car curr) 'quote) acc)
					((in-acc? curr acc) acc)
					((andmap (lambda (x) (and (list? x) (equal? 'quote (car x)))) (cdr curr)) (append acc (only-if-bigger-then-one curr expr)))
					((ormap list? curr) (if (equal? '() (only-if-bigger-then-one curr expr)) (count-repets expr curr acc) (append acc (only-if-bigger-then-one curr expr))))
				(else (append acc (only-if-bigger-then-one curr expr))))) 
				hash curr-expr)))
				(define is-car-x-in-cdr-y (lambda (check inList) (ormap (lambda (curr) 
					(cond
						((equal? curr check) #t)
						((not (list? curr)) #f)
						(else (is-car-x-in-cdr-y check curr)))) inList)))
						
(define expend-max-sym-map 
	(lambda (lst)
		(let ((expend-first (append lst (fold-left (lambda (acc x) (append (expend-max-sym-map (map car (count-repets x x '()))) acc)) '() lst))))
		expend-first)))
								
(define all-aplic (lambda (lst)
	(let* (
		(filter-map (map car (filter (lambda (x)(> (cdr x) 1)) lst)))
		(expend-sym-map (list-sort (lambda (x y) (< (length x) (length y))) (expend-max-sym-map filter-map)))
		(sym-map (map (lambda (x) 
	(cons (string->symbol (symbol->string (gensym))) `(,x)))
	 expend-sym-map)))
	 (list-sort (lambda (x y) (is-car-x-in-cdr-y (car x) (cdr y)))
		 (map (lambda (curr-x)
		 	 (if (ormap list? (car (cdr curr-x))) 
		 		(cons  (car curr-x) 
					(list (letrec ((takeMyMap (lambda(m) 
						(if (list? m)(map 
					(lambda (x)(if (and (list? x) (member x (map cadr sym-map)))
						 			(caar (filter (lambda (s) (equal? (car (cdr s)) x)) sym-map))
									 (takeMyMap x)))
				m) m))))
			(takeMyMap (car (cdr curr-x))))))
			 curr-x))
	 sym-map)))))
	 
(define replace-all (lambda (expr symbols) 
		 (map (lambda (x)
			 (cond 
				 ((not (list? x)) x)
				 ((not (equal? x (extract-symbol x symbols))) (extract-symbol x symbols))
				 ((ormap list? x) (replace-all x symbols))
			 (else (extract-symbol x symbols))))
		  expr)))
		  
(define replace-all-helper 
	(lambda (expr symbols)
		 (if (equal? expr (replace-all expr symbols)) 
		 	expr 
		(replace-all-helper (replace-all expr symbols) symbols))))

(define cse (lambda (x)
	(letrec 
		((to-replace 
			(lambda (acc exprs) 
			(append acc (all-aplic (count-repets exprs exprs '())))))
		 (expr-after 
			(lambda (exprs alredy-hash)
				(let* ((hash-map      (to-replace alredy-hash exprs))
					   (after-replace (replace-all-helper exprs hash-map)))     
					   		(if (equal?  after-replace exprs) 
								(gen-code exprs hash-map)
								(expr-after after-replace hash-map))))))
		(expr-after x '()))))