(load "compiler.scm")

(define string->exprlist 
  (lambda (string)
    (if (or (null? string) (equal? string "")) '() 
    (<sexpr> (string->list string)
	    (lambda (e s)
	      `(,e ,@(string->exprlist (list->string s))))
	    (lambda (w) `(failed with report: ,@w))))
	    ))

(define parse-expr
        (lambda (expr-list)
            (annotate-tc(pe->lex-pe(box-set(remove-applic-lambda-nil(eliminate-nested-defines(parse expr-list))))))))

(define find-const 
      (lambda (ast)
          (letrec ((foo 
                      (lambda(ast bag)
                          (cond ((or (null? ast) (not(list? ast))) '())
                                ((and (list? ast) (equal? (car ast) 'const)) (append bag (cdr ast)))
                                (else (fold-left (lambda(bag expr)(append bag (foo expr '()))) bag ast))
                            )
                      )))
            (foo ast '()))
            ))    

(define remove-dup
      (lambda (lst)
          (if (null? lst) '()
              (cons (car lst) (remove-dup (filter (lambda (x) (not (equal? x (car lst)))) 
                  
                                            (cdr lst)))))))
;if lst1 include in lst2
(define include-relation
      (lambda(lst1 lst2)
        (if (or (not (list? lst1)) (not (list? lst2))) #f
            (let ((l1 (reverse lst1))
                  (l2 (reverse lst2)))
                  (letrec ((include-list
                          (lambda (x y)
                            (cond ((and(null? x) (null? y)) #t)
                                  ((null? x) #t)
                                  ((null? y) #f)
                                  ((and (list? x) (list? y) (equal? (car x) (car y))) (include-list (cdr x) (cdr y)))
                                  (else #f)))
                          ))
                  (include-list l1 l2))
            ))))

(define topological-sort
      (lambda(const-list)
          (sort include-relation const-list)))

(define code-gen 
    (lambda (pe) 
    '(assembly lines)))
    
(define compile-scheme-file 
    (lambda (fileName assemblyTarget) 
            (let* ((file (file->string fileName))
                   (expr-list (string->exprlist file))
                   (parsed-exprs (map parse-expr expr-list))
                   (const-table (topological-sort(remove-dup(find-const (car parsed-exprs)))))
                   )
            const-table)    
            ))