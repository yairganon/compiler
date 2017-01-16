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
(define topological-sort
      (lambda(const-list)
          const-list))

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