(load "pc.scm")

(define <whitespace>
	(new (*parser (range (integer->char 0) (integer->char 32)))
		done))

(define <end-of-line/file-comment>
	(new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
               done))

(define <line-comment>
    (new (*parser (char #\;)) 
         (*parser <any-char>)
         (*parser <end-of-line/file-comment>)
         *diff *star
         (*parser <end-of-line/file-comment>)
         (*caten 3)
         done))

(define <expr-comment>
  (new (*parser (word "#;"))
  	   (*parser <whitespace>)*star
       (*delayed (lambda () <sexpr>))
       (*caten 3)
       done))

(define <inifx-expr-comment>
  (new (*parser (word "#;"))
  	   (*parser <whitespace>)*star
       (*delayed (lambda () <infix-expression>))
       (*caten 3)
       done))

(define <infix-comment>
  (new (*parser <line-comment>)
       (*parser <inifx-expr-comment>)
       (*disj 2)
       done))

(define <comment>
  (new (*parser <line-comment>)
       (*parser <expr-comment>)
       (*disj 2)
       done))
;-------------------------------boolean---------------------------	   
(define <boolean> 
	(new (*parser (char #\#))
		 (*parser (char-ci #\t))
		 (*caten 2)
		 (*pack-with
		 	(lambda (_ __) #t))
		 (*parser (char #\#))
		 (*parser (char-ci #\f))
		 (*caten 2)
		 (*pack-with
		 	(lambda (_ __) #f))
		 (*disj 2)
		 done))
;###############################boolean###########################
  
;-------------------------------char---------------------------	 
(define ^<meta-char>
	(lambda (str ch)
		(new (*parser (word-ci str))
	 		 (*pack (lambda (_) ch))
	 		  done))) 
			  
(define <char-prefix>	
	(new (*parser (char #\#))
		 (*parser (char #\\))
		 (*caten 2)
		 done))

(define <visible-simple-char> 
	(new (*parser <any-char>)
		 (*parser (range (integer->char 0) (integer->char 32)))
		 *diff
		 done))

(define <named-char> 
	(new (*parser (^<meta-char> "lambda" (integer->char 955)))
		 (*parser (^<meta-char> "newline" #\newline))
		 (*parser (^<meta-char> "nul"     #\nul))
		 (*parser (^<meta-char> "page"    #\page))
		 (*parser (^<meta-char> "return"  #\return))
		 (*parser (^<meta-char> "space"   #\space))
		 (*parser (^<meta-char> "tab"     #\tab))
		 (*disj 7)
		 done))

(define <hex-char> 
	(new (*parser (range #\0 #\9))
		 (*parser (range #\a #\f))
		 (*disj 2) 
		 done))

(define <hex-unicode-char> 
	(new (*parser (char #\x))
		 (*parser <hex-char>) *plus
		 (*caten 2)
		 (*pack-with 
		 	(lambda (_ number) (integer->char (string->number (list->string number) 16) )))
		 done))

(define <char> 
		(new (*parser <char-prefix>)
			 (*parser <hex-unicode-char>)
			 (*parser <named-char>)
			 (*parser <visible-simple-char>)
			 (*disj 3)
			 (*caten 2)
			 (*pack-with
			 	(lambda (_ char) char))
			 (*parser <visible-simple-char>)
		     (*parser (char #\)))
			 (*parser (char #\[))
			 (*disj 2)	 
			 *diff
			 *not-followed-by
			 done))		 
;###############################char###########################

;-------------------------------number---------------------------
(define <nat>
  (new (*parser (range #\0 #\9)) *plus
       (*pack-with
	   		(lambda _
				(string->number
					(list->string _))))
       done))
	  
(define <inteager>
  (new (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	   		(lambda (++ n) n))
       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	   		(lambda (-- n) (- n)))
       (*parser <nat>)
       (*disj 3)
       done)) 
	   
(define <fraction>
  (new (*parser <inteager>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))
	   
(define <number> 
	(new (*parser <fraction>)
		 (*parser <inteager>)
		 (*disj 2)
		 (*delayed (lambda () <symbol-char>))
		 *not-followed-by
		 done))
		 
(define <number-can-be-followd> 
		(new (*parser <fraction>)
		 	 (*parser <inteager>)
		 	 (*disj 2)
		     (*delayed  (lambda () <infix-symbol> ))
			 *not-followed-by
		 	 done))
		 
;###############################number###########################

;###############################string###########################
(define <string-visible-char> 
	(new (*parser <visible-simple-char>)
	     (*parser (char #\space))
		 (*disj 2)
		 done))


(define <string-meta-char> 
	(new (*parser (^<meta-char> "\\\\"  #\\))
		 (*parser (^<meta-char> "\\\""  #\"))
		 (*parser (^<meta-char> "\\t"   #\tab))
		 (*parser (^<meta-char> "\\f"   #\page))
		 (*parser (^<meta-char> "\\n"   #\newline))
		 (*parser (^<meta-char> "\\r"   #\return))
		 (*disj 6)
		 done))

(define <string-hex-char> 
	(new (*parser (word "\\x"))
		 (*parser <hex-char>) *star
		 (*parser (char #\;))
		 (*caten 3)
		 (*pack-with 
		 	(lambda (_ chars __)
				(integer->char (string->number (list->string chars) 16))))
		  done))

(define <string-char> 	
	(new
		 (*parser <string-meta-char>) 
		 (*parser <string-hex-char>)
		 (*parser <string-visible-char>)
    	 (*disj 3)
		 done))

(define <string> 
	(new (*parser (char #\"))
	     (*parser <string-char>)
		 (*parser (char #\"))
		 *diff
		 *star
		 (*parser (char #\"))
		 (*caten 3)
		 (*pack-with 
		 	(lambda (_ chars __) 
				(list->string chars)))
		 done))
;###############################string###########################


;###############################symbol###########################
(define <symbol-char> 
	(new (*parser (range #\0 #\9))
		 (*parser (range #\a #\z))
		 (*parser (range #\A #\Z))
		 (*parser (char #\!))
		 (*parser (char #\$))
		 (*parser (char #\^))
		 (*parser (char #\*))
		 (*parser (char #\-))
		 (*parser (char #\_))
		 (*parser (char #\=))
		 (*parser (char #\+))
		 (*parser (char #\<))
		 (*parser (char #\>))
		 (*parser (char #\?))
		 (*parser (char #\/))
		 (*disj 15)
		 done))
		 
(define <symbol> 
	(new (*parser <symbol-char>) *plus
		 (*pack-with (lambda _ (string->symbol (string-downcase (list->string _)))))
			done))	
		
(define ^<sexprs-delay>
	(lambda (starOrPlus)
		(new 
			(*delayed (lambda () <sexpr>)) starOrPlus
		done)))
;###############################symbol###########################
	
;###############################properList###########################
(define <properList> 	
	(new (*parser (char #\( ))
		 		 (*parser <comment>) 
		 (*parser <whitespace>) 
		 (*disj 2)
		 *star
	     (*parser (^<sexprs-delay> *star)) 
	     		 (*parser <comment>) 
		 (*parser <whitespace>) 
		 (*disj 2)
		 *star
		 (*parser (char #\) ))
		 (*caten 5)
		 (*pack-with (lambda (_ _space1 sexpr _space2 __) sexpr))
		 done))
;###############################properList###########################
		 
;###############################improperList###########################
(define <improperList> 
	(new (*parser (char #\( ))
	     (*parser (^<sexprs-delay> *plus))
		 (*parser (char #\. ))
		 (*delayed (lambda () <sexpr>)) 
		 (*parser (char #\) ))
		 (*caten 5)
		 (*pack-with (lambda (_ sexprs dot sexpr __) `(,@sexprs . ,sexpr)))
		 done))
;###############################improperList###########################
		 
;###############################vector###########################
(define <vector> 
	(new (*parser   (char #\# )   )
		 (*parser   (char #\( )   )
		 (*parser (^<sexprs-delay> *star)) 
		 (*parser   (char #\) )   )
		 (*caten 4)
		 (*pack-with (lambda (_ __ sexpr ___) (list->vector sexpr)))
		done))
;###############################vector###########################

;###############################quoted###########################
(define <quoted> 
	(new (*parser (char #\'))
		  (*delayed (lambda () <sexpr>)) 
		 (*caten 2)
		 (*pack-with (lambda (_ sexpr)    `',sexpr))
		done))
;###############################quoted###########################

;###############################quasiQuoted###########################
(define <quasiQuoted> 
	(new (*parser (char #\`))
		  (*delayed (lambda () <sexpr>)) 
		 (*caten 2)
		 (*pack-with (lambda (_ sexpr)    (list 'quasiquote sexpr)))
		done))
;###############################quasiQuoted###########################

;###############################unquoted###########################
(define <unquoted> 
		(new (*parser (char #\,))
		  (*delayed (lambda () <sexpr>)) 
		 (*caten 2)
		 (*pack-with (lambda (_ sexpr)   (list 'unquote sexpr)))
		done))
;###############################unquoted###########################

;###############################unquoteAndSpliced###########################

(define <unquoteAndSpliced> 
	(new 
		(*parser (char #\, ))
    	(*parser (char #\@ ))
   	    (*delayed (lambda () <sexpr>)) 
		(*caten 3)
		(*pack-with (lambda (_ __ sexpr)   (list'unquote-splicing sexpr)))
		done))
;###############################unquoteAndSpliced###########################

;###############################infixExtension###########################
(define <clean-shit-parser>
	(new (*parser <infix-comment>) 
	 	 (*parser <whitespace>) 
	 	 (*disj 2)
	 	 *star
	done))
	
(define <neg-sym-handle>
	(lambda(sym-parser)
		(new
			(*parser (char #\-))
			(*parser <whitespace>) *plus
			(*parser <clean-shit-parser>)
			(*parser <number-can-be-followd>)
			(*caten 4)
			(*pack-with (lambda (neg __ _ num) `(- ,num)))
			(*parser (char #\-))
   		    (*parser <clean-shit-parser>)
			(*parser sym-parser)
			(*caten 3)
			(*pack-with (lambda (neg _ sym) `(- ,sym)))
			(*parser sym-parser)
			(*disj 3)	
			done))
		)
			
(define ^<infix->simple-op> 
	(lambda (op1 op1-q op2 op2-q <next-parser>)
		(new (*parser <next-parser>) 
     	     (*parser (^<meta-char> op1 op1-q))
		     (*parser (^<meta-char> op2 op2-q))
		     (*disj 2)
			 (*parser <next-parser>)
			 (*caten 2)
			 (*pack-with (lambda (op sec-val)
			 				(lambda (first-val)
								`(,op ,first-val ,sec-val))))
			 *star
			 (*caten 2)
			 (*pack-with (lambda (first-val func-list)
				 			(fold-left (lambda (acc f) (f acc)) first-val func-list)))
	         done)))
			
(define ^<prefix-suffix>
	(lambda (<prefix> <sefix> func)
		(new
			(*parser <prefix>)
			(*parser <sefix>)
			(*caten 2)
			(*pack-with func)
		done)))	
		
(define ^<prefix-suffix-plus>
	(lambda (<sefix>)
		(new
			(*parser <infix->atom>)
			(*parser <sefix>) *plus
			(*caten 2)
			(*pack-with (lambda (sym lmd) (fold-left (lambda (elm func) (func elm)) sym lmd)))
		done)))	
				  
(define <infix-prefix-extension-prefix>
	(new (*parser (word "##"))
		 (*parser (word "#%"))
		 (*disj 2)
	done))

(define <infix-symbol> 
	(new (*parser <symbol-char>)
	     (*parser (char #\*))
		 (*parser (char #\+))
		 (*parser (char #\-))
		 (*parser (char #\^))
		 (*parser (char #\/))
		 (*disj 5)
		 *diff
		 *plus
		 (*pack-with (lambda _ (string->symbol (string-downcase (list->string _)))))
			done))

(define <infix->paren>
	(new (*parser (char #\())
		 (*delayed (lambda () <infix->+->))
	 	 (*parser (char #\)))
		 (*caten 3)
		 (*pack-with (lambda (_ infix-exp __) infix-exp))
	done))

(define <infix-sexpr-escape> 
  (new 
    (*parser <infix-prefix-extension-prefix>)
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (_ __) __))
    done))

(define <infix->atom>
		(new (*parser <clean-shit-parser>)
			 (*parser (<neg-sym-handle> <infix->paren>))
			 (*parser  <number-can-be-followd>)
			 (*parser (<neg-sym-handle>  <infix-symbol>))
			 (*parser <infix-sexpr-escape>)
			 (*disj 4)
			 (*parser <clean-shit-parser>)
			 (*caten 3)
			 (*pack-with (lambda(_ x __) x))
			 done))
	
(define <infix->array-pos>
	(new (*parser (char #\[))
		 (*delayed (lambda () <infix->+->))
		 (*parser (char #\]))
		 (*caten 3)
	     (*pack-with (lambda (_  data __)
	                   (lambda (sym) `(vector-ref ,sym ,data))))
	done))		
		 
(define <infix-arg-list>
	(new 
 		 (*parser (char #\())
		 (*delayed (lambda () <infix->+->))
		 (*parser (char #\,))
	     (*delayed (lambda () <infix->+->))
		 (*caten 2)
		 (*pack-with (lambda (_ var) var))
		 *star
		 (*caten 2)
		 (*pack-with (lambda (arg args) `(,arg ,@args)))
		 			(*parser <clean-shit-parser>)

		 (*parser <epsilon>)
		 			(*parser <clean-shit-parser>)
		 			(*caten 3)
		 			 (*pack-with (lambda (_ eps __) eps))
		 (*disj 2)
		 (*parser (char #\)))
		 (*caten 3)
	 	 (*pack-with (lambda (_ body __) 
		 				(lambda (sym) `(,sym ,@body))))
	done))

(define <infix->^>
	(new (*delayed (lambda ()<infix->last>))
		 (*parser (^<meta-char> "^" 'expt))
		 (*parser (^<meta-char> "**" 'expt))
		 (*disj 2)
		 (*delayed (lambda () <infix->last>))
		 (*caten 2)
		 (*pack-with (lambda (_ sec-val) sec-val))
		 *star
		 (*caten 2)
		 (*pack-with (lambda (first-val power-list)
			 (if (null? power-list) first-val
			 (letrec ((power-fold-left (lambda (lst)
				 `(expt ,(car lst) ,(if(equal? 2 (length lst)) (cadr lst) (power-fold-left (cdr lst)))))))
				 (power-fold-left `(,first-val ,@power-list))))))
			 done))	
				 	
(define <infix->array> (^<prefix-suffix-plus> <infix->array-pos>))
	 
(define <infix->func>  (^<prefix-suffix-plus> <infix-arg-list>))	
	
(define <func->array>  (^<prefix-suffix>  <infix->func> <infix->array-pos> (lambda (sym lmd) (lmd sym))))

(define <array->func>  (^<prefix-suffix>  <infix->array> <infix-arg-list> (lambda (array  args) (args array))))

(define <infix->*/> (^<infix->simple-op> "*" '* "/" '/ <infix->^>))
		
(define <infix->+-> (^<infix->simple-op> "+" '+ "-" '- <infix->*/>))	 	 	 					

(define <infix->last>
	(new 
		 (*parser  <clean-shit-parser>)
		 (*parser  (<neg-sym-handle> <array->func>))
	     (*parser  (<neg-sym-handle> <func->array>))
	     (*parser  (<neg-sym-handle> <infix->array>))
	     (*parser  (<neg-sym-handle> <infix->func>))
		 (*parser   <infix->atom>)
		 (*disj 5)
		 (*parser <clean-shit-parser>)
		 (*caten 3)
		 (*pack-with (lambda(_ x __) x))
	 done))
	 
(define <infix-expression> 
	(new (*parser <infix->+->)
	done))

(define <infixExtension> 
	(new (*parser <infix-prefix-extension-prefix>)
		 (*parser <clean-shit-parser>)
		 (*parser <infix-expression>)
		 (*caten 3)
		 (*pack-with (lambda (prefix _ exp) exp))
	done))
;###############################infixExtension###########################
(define ^<sexpr> 
	(new 
		 (*parser <boolean>)
		 (*parser <char>)
		 (*parser <number>)
		 (*parser <string>)
		 (*parser <symbol>)
		 (*parser <properList>)
		 (*parser <improperList>)
		 (*parser <vector>)
		 (*parser <quoted>)
		 (*parser <quasiQuoted>)
		 (*parser <unquoted>)
		 (*parser <unquoteAndSpliced>)
		 (*parser <infixExtension>)
		 (*disj 13)
		 done))
		 
(define <sexpr> 
	(new (*parser <comment>) 
		 (*parser <whitespace>) 
		 (*disj 2)
		 *star
   		 (*parser ^<sexpr>)
		 (*parser <comment>) 
		 (*parser <whitespace>) 
		 (*disj 2)
		 *star
		 (*caten 3)
		 (*pack-with (lambda (_ sexpr __) sexpr))
		 done))



		
		 
		



;---------------------------------HW2------------------------------------------------------------------------




(load "pattern-matcher.scm")
(define *reserved-words* '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))
(define *void-object* (void))
(define simple-const? (lambda (c)(or (null? c) (boolean? c) (char? c) (number? c) (string? c) (vector? c) )))

	(define var? 
	(lambda (x)
	(and (symbol? x) (not (member x *reserved-words*)))))
	(define var-list? (lambda (lst) (and (list? lst) (andmap var? lst))))
	
	(define identify-lambda-helper
		(lambda (ret-simple ret-opt ret-var) 
			(lambda (argl)
				(cond 
					((null? argl) (ret-simple '()))
					((var? argl) (ret-var argl))      
					(else ((identify-lambda-helper 
							(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
							(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
							(lambda (var) (ret-opt `(,(car argl)) var)))(cdr argl))
						)))))
							
	(define identify-lambda  (identify-lambda-helper (lambda (s) `(lambda-simple ,s)) (lambda (s opt) `(lambda-opt ,s ,opt)) (lambda (var) `(lambda-var ,var))))		
	(define beginify
		(lambda (s)
			(cond
				((null? s) *void-object*)
				((null? (cdr s)) (car s))
				(else `(begin ,@s)))))
		
        (define clear-b 
            (lambda (l)
                (fold-right (lambda(curr acc) 
                                (if (and (list? curr) (equal? (car curr) 'begin))
                                    (append (clear-b (cdr curr)) acc) 
                                    (cons curr acc))) 
                            '() 
                            l)))        
	(define get-vars (lambda (list) (map car list)))	
	(define get-vals (lambda (list) (map cadr list)))	
	(define letrec-set (lambda (list) (map (lambda (x) `(set! ,(car x) ,(cadr x))) list)))
	(define get-false (lambda (list) (map (lambda (x) #f) list)))
	(define ^quote?
	  (lambda (tag)
	    (lambda (e)
	      (and (pair? e)
		   (eq? (car e) tag)
		   (pair? (cdr e))
		   (null? (cddr e))))))

	(define quote? (^quote? 'quote))
	(define unquote? (^quote? 'unquote))
	(define unquote-splicing? (^quote? 'unquote-splicing))

	(define const?
	  (let ((simple-sexprs-predicates
		 (list boolean? char? number? string?)))
	    (lambda (e)
	      (or (ormap (lambda (p?) (p? e))
			 simple-sexprs-predicates)
		  (quote? e)))))

	(define quotify
	  (lambda (e)
	    (if (or (null? e)
		    (pair? e)
		    (symbol? e)
		    (vector? e))
		`',e
		e)))

	(define unquotify
	  (lambda (e)
	    (if (quote? e)
		(cadr e)
		e)))

	(define const-pair?
	  (lambda (e)
	    (and (quote? e)
		 (pair? (cadr e)))))

	(define expand-qq
	  (letrec ((expand-qq
		    (lambda (e)
		      (cond ((unquote? e) (cadr e))
			    ((unquote-splicing? e)
			     (error 'expand-qq
			       "unquote-splicing here makes no sense!"))
			    ((pair? e)
			     (let ((a (car e))
				   (b (cdr e)))
			       (cond ((unquote-splicing? a)
				      `(append ,(cadr a) ,(expand-qq b)))
				     ((unquote-splicing? b)
				      `(cons ,(expand-qq a) ,(cadr b)))
				     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
			    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
			    ((or (null? e) (symbol? e)) `',e)
			    (else e))))
		   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
		   (optimizer
		    (compose-patterns
		     (pattern-rule
		      `(append ,(? 'e) '())
		      (lambda (e) (optimize-qq-expansion e)))
		     (pattern-rule
		      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
		      (lambda (c1 c2 e)
			(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
			      (e (optimize-qq-expansion e)))
			  (optimize-qq-expansion `(append ,c ,e)))))
		     (pattern-rule
		      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
		      (lambda (c1 c2)
			(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
			  c)))
		     (pattern-rule
		      `(append ,(? 'e1) ,(? 'e2))
		      (lambda (e1 e2)
			(let ((e1 (optimize-qq-expansion e1))
			      (e2 (optimize-qq-expansion e2)))
			  `(append ,e1 ,e2))))
		     (pattern-rule
		      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
		      (lambda (c1 c2 e)
			(let ((c (quotify (list (unquotify c1) (unquotify c2))))
			      (e (optimize-qq-expansion e)))
			  (optimize-qq-expansion `(append ,c ,e)))))
		     (pattern-rule
		      `(cons ,(? 'e1) ,(? 'e2))
		      (lambda (e1 e2)
			(let ((e1 (optimize-qq-expansion e1))
			      (e2 (optimize-qq-expansion e2)))
			  (if (and (const? e1) (const? e2))
			      (quotify (cons (unquotify e1) (unquotify e2)))
			      `(cons ,e1 ,e2))))))))
	    (lambda (e)
	      (optimize-qq-expansion
	       (expand-qq e)))))
		   
		   
		   
		   			
	(define parse
		(let ((run 
			(compose-patterns
				
				(pattern-rule
					`(quasiquote ,(? 'l) . ,(? 'rest))
				(lambda (l rest) (parse (expand-qq (append l rest)))))
				
				(pattern-rule
					`(let ()  ,(? 'body) . ,(? 'rest))
				(lambda (body rest) (parse `((lambda () ,(beginify (cons body rest)))))))
				
				(pattern-rule
					`(let* () ,(? 'body) . ,(? 'rest))
				(lambda (body rest) (parse `((lambda () ,(beginify (cons body rest)))))))
				
				(pattern-rule
					`(letrec ()  ,(? 'body) . ,(? 'rest))
				(lambda (body rest) (parse `((lambda () ((lambda () ,(beginify (cons body rest)))))))))
								
				(pattern-rule
					`(let ,(? 'var-vals) . ,(? 'exprs))
				(lambda (var-vals exprs) (parse `((lambda ,(get-vars var-vals) ,@exprs) ,@(get-vals var-vals)))))
				;;let*
				
				(pattern-rule
					`(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
				(lambda (var val rest exprs) 
					(let ((body (if (null? rest) (beginify exprs) `(let* ,rest . ,exprs))))
					(parse `(let ((,var ,val)) ,body)))))
				
				(pattern-rule
					`(letrec ,(? 'var-vals) . ,(? 'exprs))
				(lambda (var-vals exprs) (parse `((lambda ,(get-vars var-vals) ,@(letrec-set var-vals) ((lambda () ,@exprs))) ,@(get-false var-vals)))))
				;;lambda
				(pattern-rule
					`(lambda ,(? 'args)  ,(? 'body) . ,(? 'rest))
				(lambda (args body rest) `(,@(identify-lambda args) ,(parse (beginify (cons body rest))))))
				
				;;lambda
				
				(pattern-rule
					`(set! ,(? 'var var?) ,(? 'val))
				(lambda (var val) `(set ,(parse var) ,(parse val))))
				
				(pattern-rule
					`(begin ,(? 'first))
				(lambda (first) (parse first)))
				
				(pattern-rule
					`(begin)
				(lambda () `(const ,*void-object*)))
				
				(pattern-rule
					`(begin . ,(? 'rest))
				(lambda (rest) `(seq  (,@(map parse (clear-b rest))))))
				
				(pattern-rule
					`(cond ,(? 'first list?))
				(lambda (first) (parse `(if ,(car first) ,(beginify (cdr first))))))
				(pattern-rule
					`(cond ,(? 'first list?) (else ,(? 'die) . ,(? 'rest-die)))
				(lambda (first die rest-die) (parse `(if ,(car first) ,(beginify (cdr first)) ,(beginify (cons die rest-die))))))
				(pattern-rule
					`(cond ,(? 'first list?) . ,(? 'rest))
				(lambda (first rest) (parse `(if ,(car first) ,(beginify (cdr first)) (cond ,@rest)))))
				
				(pattern-rule
					`(define ,(? 'mit-define pair?) ,(? 'body) . ,(? 'rest-body))
				(lambda (mit-define body rest-body) (parse `(define ,(car mit-define) (lambda ,(cdr mit-define) ,(beginify (cons body rest-body)))))))

				(pattern-rule
					`(define ,(? 'var var?) ,(? 'val) . ,(? 'rest-body))
				(lambda (var val rest-body) `(def ,(parse var) ,(parse (beginify (cons val rest-body))))))
				;; and
				(pattern-rule
					`(and)
				(lambda () `(const #t)))
				(pattern-rule
					`(and ,(? 'expr))
				(lambda (expr) (parse expr)))
				(pattern-rule
					`(and ,(? 'first) ,(? 'sec))
				(lambda (first sec) (parse `(if ,first ,sec ,#f))))
				(pattern-rule
					`(and ,(? 'first) . ,(? 'rest))
				(lambda (first rest) (parse `(if ,first (and ,@rest) ,#f))))			
				;; and
				(pattern-rule
					(? 'c simple-const?)
				(lambda (c) `(const ,c)))
				
				(pattern-rule
					`(quote ,(? 'c))
				(lambda (c) `(const ,c)))
				
				(pattern-rule
					(? 'v var?)
				(lambda (v) `(var ,v)))
				
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit))
				(lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
				
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
				(lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				
				(pattern-rule
					`(or)
				(lambda () (parse #f)))
				
				(pattern-rule
					`(or ,(? 'expr))
				(lambda (expr) (parse expr)))
				
				(pattern-rule
					`(or . ,(? 'exprs list?))
				(lambda (exprs) `(or ,(map parse exprs))))
				
				(pattern-rule
					`(,(? 'applic) . ,(? 'val))
				(lambda (applic val) `(applic ,(parse applic) ,(map parse val))))
				
				;; add more rules here
			)))
			(lambda (e)
				(run e
					(lambda ()
						(error 'parse
						(format "I can't recognize this: ~s" e)))))))
						
						
						
						(define yair-test (lambda(x)
							(annotate-tc (pe->lex-pe (box-set (remove-apllic (eliminate-nested-defines (parse x))))))
							))
						
						
						(define lambda-name   (lambda (lambda-expr) (car lambda-expr)))
						(define lambda-body   (lambda (lambda-expr) (if (equal? (car lambda-expr) 'lambda-opt) (cdddr lambda-expr) (cddr lambda-expr))))
						(define lambda-params (lambda (lambda-expr) (if (equal? (car lambda-expr) 'lambda-opt) `(,(cadr lambda-expr) ,(caddr lambda-expr))  `(,(cadr lambda-expr)))))
						(define lambda? (lambda (x)
							(and (list? x) (not (null? x)) 
								 (or (equal? (car x) 'lambda-simple)
									 (equal? (car x) 'lambda-opt)
									 (equal? (car x) 'lambda-var)))))
			 
						;eliminate-nested-defines-helper		
						(define ds+res (lambda (def expr)
							(if (null? def) (if (equal? (length expr) 1) expr `((seq ,expr)))
							`((applic (lambda-simple ,(map cadadr def) (seq (,@(map (lambda (x) (cons 'set (cdr x))) def) ,@expr))) ,(map (lambda (x) '(const #f)) def))))))

						(define eliminate-nested-defines (lambda (sexpr)
							(let ((lambda-expr (lambda (expr) 
								(if (lambda? expr)
									`(,(lambda-name expr) ,@(lambda-params expr) ,@(eliminate-nested-defines (eliminate-nested-defines-helper (lambda-body expr) ds+res)))
							 		expr))))
							(cond ((lambda? sexpr) (lambda-expr sexpr))
								  ((list? sexpr) (map eliminate-nested-defines sexpr))
							  (else (lambda-expr sexpr))))))

						(define eliminate-nested-defines-helper 
							(lambda (parse-expr ret-ds+res)
								(if (null? parse-expr) (ret-ds+res '() '())
									(eliminate-nested-defines-helper (cdr parse-expr)
									(lambda (ds es)
										(cond ((eq? (caar parse-expr) 'def)
													(ret-ds+res (cons (eliminate-nested-defines (car parse-expr)) ds) es))
											  ((eq? (caar parse-expr) 'seq)
											   		(eliminate-nested-defines-helper (cadar parse-expr)
														(lambda (ds1 es1)
															(ret-ds+res 
																(append ds1 ds)
																(append (eliminate-nested-defines es1) es)))))
											   (else (ret-ds+res ds (cons (eliminate-nested-defines (car parse-expr)) es )))))))))
						;############################################################################################################################################################

						;remove-applic-lambda-nil
						(define remove-apllic 
							(lambda (applic-expr)
							(if (and (list? applic-expr)
							(not (null? applic-expr))
						(equal? (car applic-expr) 'applic)(lambda? (cadr applic-expr))(null? (car (lambda-params (cadr applic-expr)))) (andmap null? (caddr applic-expr)))
							 		(remove-applic-lambda-nil (car (lambda-body (cadr applic-expr))))
									applic-expr)))
	
						(define remove-applic-lambda-nil 
							(lambda (parse-expr)
								(map (lambda (x) (if (and (list? x)(ormap list? x)) (remove-applic-lambda-nil x) (remove-apllic x))) (remove-apllic parse-expr))
							))
						;############################################################################################################################################################

						;box-set
						(define give-false (lambda (x y) #f))
						(define is-bound-occurrence-body?
						 	(lambda (param body ? ?-ret ?? ??-ret)
								(cond ((or (not (list? body)) (null? body)) #f)
									  ((? body param) (if (not ?-ret) (is-bound-occurrence-body? param (cddr body) ? ?-ret ?? ??-ret) ?-ret))
									  ((?? body param) ??-ret)
								  (else (ormap 
								  	(lambda (x) 
										(if (lambda? x) 
											(is-bound-occurrence? param x ? ?-ret ?? ??-ret)
											(is-bound-occurrence-body? param x ? ?-ret ?? ??-ret)))
										 body)))))
		
						(define is-bound-occurrence? 
							(lambda (param lambda-expr ? ?-ret ?? ??-ret)
								(let ((params-to-check (lambda-params lambda-expr))
									  (body-to-check   (lambda-body lambda-expr)))
									  (and (not (member param (give-me-fucking-params-list params-to-check)))
									  	   (is-bound-occurrence-body? param body-to-check ? ?-ret ?? ??-ret)))))
				   
						(define is-bound? 
							(lambda (param body)
								(cond ((or (not (list? body)) (null? body)) #f)
									  ((lambda? body) (is-bound-occurrence? param body (lambda (body param) (and (equal? (car body) 'var) (equal? (cadr body) param))) #t give-false #f))
								  (else (ormap (lambda (x) (is-bound? param x)) body)))))
		  
						(define is-set? 
							(lambda (param lambda-expr)
								(is-bound-occurrence-body? 
									param 
									lambda-expr 
									(lambda (body param)(and (equal? (car body) 'set) (equal? (cadr (car (cdr body))) param))) 
									#t 
									give-false 
									#f)))	
			  		  			
						(define is-somewhere?
							(lambda (param lambda-expr)
								(is-bound-occurrence-body? 
									param 
									lambda-expr 
									(lambda (body param) (and (equal? (car body) 'set) (equal? (cadr (car (cdr body))) param))) 
									#f 
									(lambda (body param) (and (equal? (car body) 'var) (equal? (cadr body) param))) 
									#t)))
			
						(define replace-all (lambda (body param)
							(cond ((or (null? body) (not (list? body))) body)
								  ((and (lambda? body) (member param (give-me-fucking-params-list (lambda-params body)))) body)
								  ((or (and (equal? (car body) 'box-set) (equal? (cadr (car (cdr body))) param)) (and (equal? (car body) 'box-get) (equal? (cadr (car (cdr body))) param))) body)
								  ((and (equal? (car body) 'set) (equal? (cadr (car (cdr body))) param)) `(box-set (var ,param) ,(replace-all (car (cdr (cdr body))) param)))
								  ((and (equal? (car body) 'var) (equal? (cadr body) param)) `(box-get (var ,param)))
							  (else (map (lambda (x) (replace-all x param)) body)))))
	  			
						(define make-body 
							(lambda (body params bound-params)
								(let ((boxing-list (filter list? 
											(map (lambda (p is-bound?) 
													(if is-bound? `(set (var ,p) (box (var ,p))) 0)) params bound-params)))
									  (replaced-body (fold-left (lambda (curr-body p) (replace-all curr-body p)) (car body) (map car (filter cdr (map cons params bound-params))))))
									(cond ((null? boxing-list) body)
										   ((equal? (caar body) 'seq) `((seq (,@boxing-list ,@(car (cdr replaced-body))))))
									   (else `((seq (,@boxing-list ,replaced-body)))))
								)))
		
						(define give-me-fucking-params-list
							(lambda (params)
								(cond ((and (list? (car params)) (not (null? (cdr params)))) (append (car params) (cdr params)))
									  ((list? (car params)) (car params))
								(else params))))
				
						(define box-set 
							(lambda (expr)
								(if (or (null? expr) (not (list? expr))) expr
								(map box-set 	
								(if (lambda? expr)
									(let* ((l-name (lambda-name expr))
										  (body (lambda-body expr))
										  (params (lambda-params expr))
									 	  (bound-params (map (lambda (param) (and (is-somewhere? param body) (is-set? param expr) (is-bound? param (lambda-body expr)))) (give-me-fucking-params-list (lambda-params expr)))))		
									`(,l-name ,@params ,@(make-body body (give-me-fucking-params-list params) bound-params))) 	  
								expr)))))

						;############################################################################################################################################################

						;pe->lex-pe
						(define pe->lex-pe 
							(lambda (expr)
								(analyse expr '() '() #f)))
		
						(define set-bound-var 
							(lambda (var scope)
								(let* ((scope-list (map (lambda (x) (if (member var x) (- (length x) (length (member var x))) #f)) scope))
									   (minor (find number? scope-list))
								   	   (major (- (length scope-list) (length (member minor scope-list)))))
								   `(bvar ,var ,major ,minor))))	
		
						(define analyse 
							(lambda (ast s-scope params not-first-time)
								(let* ((updated-s-cope (if (and not-first-time (lambda? ast)) (cons params s-scope) s-scope))
									   (updated-params (if (lambda? ast) (give-me-fucking-params-list (lambda-params ast)) params)))
									   (cond ((or (null? ast) (not (list? ast))) ast)
									   		 ((equal? (car ast) 'var)
										   	  		(cond 
														((member (cadr ast) updated-params) `(pvar ,(cadr ast) ,(- (length updated-params) (length (member (cadr ast) updated-params)))))
														((ormap (lambda (x) (member (cadr ast) x)) updated-s-cope) (set-bound-var (cadr ast) updated-s-cope))
										   		  	(else `(fvar ,(cadr ast)))))
											(else (map (lambda (x) (analyse x updated-s-cope updated-params #t)) ast))))))	
					
					
						;############################################################################################################################################################
	
						;annotate-tc
						(define handle-or 
							(lambda (l tp?)
								(if (or (null? l) (null? (cdr l))) 
									(annotate l tp?)
								(cons (annotate (car l) #f)(handle-or (cdr l) tp?)))))
		
						(define annotate 
							(lambda (expr tp?)
								(cond ((or (null? expr) (not (list? expr)) (equal? (car expr) 'var) (equal? (car expr) 'const)) expr)
									   ((equal? (car expr) 'applic) (if tp? `(tc-applic ,@(annotate (cdr expr) #f)) `(applic ,@(annotate (cdr expr) #f))))
									   ((lambda? expr) `(,(lambda-name expr) ,@(lambda-params expr) ,@(annotate (lambda-body expr) #t))) 
									   ((equal? (car expr) 'def) `(def ,(cadr expr) ,@(annotate (cddr expr) #f)))
									   ((equal? (car expr) 'set) `(set ,(cadr expr) ,@(annotate (cddr expr) #f)))
									   ((equal? (car expr) 'box-set) `(box-set ,(cadr expr) ,@(annotate (cddr expr) #f)))
									   ((equal? (car expr) 'if3) `(if3 ,(annotate (cadr expr) #f)  ,(annotate (caddr expr) tp?) ,@(annotate (cdddr expr) tp?)))
									   ((equal? (car expr) 'or)  `(or ,(handle-or (cadr expr) tp?)))
									   ((equal? (car expr) 'seq) `(seq ,(handle-or (cadr expr) tp?)))
									(else (map (lambda (x) (annotate x tp?)) expr)))))
			
						(define annotate-tc 
							(lambda (expr)
								(annotate expr #f)
							))
							