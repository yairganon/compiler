(load "compiler.scm")


(define file->string
(lambda (in-file)
(let ((in-port (open-input-file in-file)))
(letrec ((run
(lambda ()
(let ((ch (read-char in-port)))
(if (eof-object? ch)
(begin
(close-input-port in-port)
'())
(cons ch (run)))))))
(list->string
(run))))))





(define code-gen 
    (lambda (pe) 
    ;string that contains lines of assembly instructions in the CISC architecture, such that when the execution of these instructions is complete,the value of e should be in the result register R0.
    '(assembly lines)))
    
(define compile-scheme-file 
    (lambda (fileName assemblyTarget) 
            ((let* (file (file->string fileName))
                   (exprList (string->exprList file))
                   
                  )
            
            
            
            ))