(load "compiler.scm")

(define remove-dup
      (lambda (lst)
          (if (null? lst) '()
              (cons (car lst) (remove-dup (filter (lambda (x) (not (equal? x (car lst)))) 
                                            (cdr lst)))))))

(define add-prologue ;--                                             --add-prologue--                                   
      (lambda(assembly-code)
        (string-append "#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include \"cisc.h\"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"

 CONTINUE:\n" assembly-code)))

(define add-epilogue ;--                                              --add-epilogue--
      (lambda(assembly-code)
        (string-append  assembly-code " STOP_MACHINE;

  return 0;
}
")))

(define string->file ;--                                              --string->file--
  (lambda (to-write filename)
    (let ((output (open-output-file filename))
         (input (string->list to-write)))
      (letrec ((run
    (lambda (input)
      (if (equal? '() (cdr input)) (close-output-port output) 
      (let ((e (car input)))
      (write-char e output)
      (run (cdr input)))))  ))
        (run input) )  ))) 
        
(define string->exprlist ;--                                        --string->exprlist--
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

(define find-const   ;--                                              --find-const--
      (lambda (ast)
          (letrec ((foo 
                      (lambda (ast)
                          (cond ((or (null? ast) (not (list? ast))) '())
                                ((and (list? ast) (equal? (car ast) 'const)) (cdr ast))
							(else (fold-left (lambda(bag expr)(append bag (foo expr))) '() ast))))))
            (foo ast))))    

;if lst1 include in lst2
(define include-relation     ;--                                --include-relation--
      (lambda(lst1 lst2)
        (if (or (not (list? lst1)) (not (list? lst2))) #t
            (let ((l1 (reverse lst1))
                  (l2 (reverse lst2)))
                  (letrec ((include-list
                          (lambda (x y)
                            (cond ((and(null? x) (null? y)) #t)
                                  ((null? x) #t)
                                  ((null? y) #f)
                                  ((and (list? x) (list? y) (equal? (car x) (car y))) (include-list (cdr x) (cdr y)))
                                  (else #f)))))
                  (include-list l1 l2))))))

(define topological-sort  ;--                                   --topological-sort--
      (lambda(const-list)
          (sort include-relation const-list)))



(define code-gen 
    (lambda (pe)
      (cond ((equal? #f (cadr pe)) "SOB_FALSE\n") ;bla
            ((equal? #t (cadr pe)) "SOB_TRUE\n")  ;bla
            (else "#assembly-lines#\n"))))
    
(define compile-scheme-file 
    (lambda (fileName assemblyTarget) 
            (let* ((file (file->string fileName))
                   (expr-list (string->exprlist file))
                   (parsed-exprs (map parse-expr expr-list))
				           (const-table (topological-sort(remove-dup(find-const parsed-exprs))))
                   (assembly-code (fold-left string-append "" (map code-gen parsed-exprs)))
                   (output (add-epilogue (add-prologue assembly-code)))
                   (temp-void (string->file output assemblyTarget))
                   )
                     'Done)  
              
            ))
			(display (compile-scheme-file "source.scm" "target.c"))
			(newline)
			