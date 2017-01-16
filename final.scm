(load "compiler.scm")


(define string->exprlist 
  (lambda (string)
    (if (or (null? string) (equal? string "")) '() 
    (<sexpr> (string->list string)
	    (lambda (e s)
	      `(,e ,@(string->exprlist (list->string s))))
	    (lambda (w) `(failed with report: ,@w))))
	    ))
(define parse_expr
        (lambda (expr_list)
            (annotate-tc(pe->lex-pe(box-set(remove-applic-lambda-nil(eliminate-nested-defines(parse expr_list))))))))
            
(define code-gen 
    (lambda (pe) 
    ;string that contains lines of assembly instructions in the CISC architecture, such that when the execution of these instructions is complete,the value of e should be in the result register R0.
    '(assembly lines)))
    
(define compile-scheme-file 
    (lambda (fileName assemblyTarget) 
            (let* ((file (file->string fileName))
                   (exprList (string->exprlist file))
                   (parsed_exprs (map parse_expr exprList))
                   )
            parsed_exprs)
            
            
            ))