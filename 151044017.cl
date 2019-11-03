(setq token_map '())
(setq buffer '())
(setq pairOperator '("operator" 0))
(setq pairKeyword '("keyword" 0))
(setq pairInteger '("integer" 0))
(setq pairBinary '("binary" 0))
(setq pairIdentifier '("identifier" 0))
(setq c "!")

(defun lexer (filename)
	(tokenify (readWords filename))
	(mapcar 'list (even-indices token_map) (odd-indices token_map)) 
)

(defun tokenify (L)
	(setq token_map '())
	(loop for i in L
	    when (isOperator i) ;DFA for operator
	      	do (progn 
	      			(setq c (string i))
	      			(setf (nth 0 pairOperator) "operator")
	      			(setf (nth 1 pairOperator) c)
	      			(setq token_map (append token_map pairOperator))
	      			(setq pairOperator '(0 0))	
	      		)
	    else
	    	do (progn
	    			(if (and (not (char= #\Space i )) (not (char= #\Tab i)) (not (char= #\newline i)) ) 
	    				(setq buffer (append buffer (list i)))
	    				(if (and (not (equal nil buffer)) (not (char= (first (last buffer)) #\Space)))
	    					(progn 
	    						(setq buffer (coerceListOfChars buffer))
	    						(cond 
	    							((isKeyword (list buffer))
	    								(setf (nth 1 pairKeyword) buffer)
	    								(setq token_map (append token_map pairKeyword))
	    								(setf buffer '())
	    							)
	    							((isBinary (list buffer))
	    								(setf (nth 1 pairBinary) buffer)
	    								(setq token_map (append token_map pairBinary))
	    								(setf buffer '())
	    							)

	    							((isInteger (coerce buffer 'string))
	    								(setf (nth 1 pairInteger) buffer)
	    								(setq token_map (append token_map pairInteger))
	    								(setf buffer '())
	    							)
	    							(T
	    								(setf (nth 1 pairIdentifier) buffer)
	    								(setq token_map (append token_map pairIdentifier))
	    								(setf buffer '())
	    							)
	    						)
	    					)
							nil
	    				)
	    			)
	    		)
	    end)
)

(defun isOperator (token)
	(cond 
		( (char= #\( token) T)
		( (char= #\) token) T)
		( (char= #\+ token) T)
		( (char= #\- token) T)
		( (char= #\* token) T)
		( (char= #\/ token) T)
		(nil)
	)
)

(defun isIdentifier (token)
	T
)

;Case sensitive keyword checker
(defun isKeyword (token)
	(cond 
		( (equal nil token) nil)
		( (equal '("and") token) T)
		( (equal '("or") token) T)
		( (equal '("not") token) T)
		( (equal '("equal") token) T)
		( (equal '("append") token) T)
		( (equal '("concat") token) T)
		( (equal '("set") token) T)
		( (equal '("deffun") token) T)
		( (equal '("for") token) T)
		( (equal '("while") token) T)
		( (equal '("if") token) T)
		( (equal '("exit") token) T)
		(nil)
	)	
)

(defun isBinary (token)
	(cond 
		( (equal nil token) nil)
		( (equal '("true") token) T)
		( (equal '("false") token) T)
		(nil)
	)	
)

(defun isInteger (i)
	(isNumeric i)
)

;; HELPER FUNCTIONS

;Input: filename
;Output: List of chars in the file i.e: (#\H #\E #\L #\L #\O #\!)
(defun readWords (filename)
    (with-open-file (stream filename)
      	(loop while (peek-char nil stream nil nil)
           	collect (read-char stream)
        )
    )
)

(defun even-indices (L)
	(setq evenList '())
	(loop for i from 0 to (- (list-length L) 1)
		do (progn
				(if (= 0 (mod i 2))
					(setq evenList (append evenList (list (nth i L))))
					
				)
			)
	)
	evenList
)

(defun odd-indices (L)
	(setq oddList '())
	(loop for i from 0 to (- (list-length L) 1)
		do (progn
				(if (= 1 (mod i 2))
					(setq oddList (append oddList (list (nth i L))))
					
				)
			)
	)
	oddList
)

;Input: List
(defun coerceListOfChars (L)
	(if(not (eq nil L))
		(coerce L 'string)
		nil
	)
)

(defun isNumeric (token)
  	(let ((*read-eval* nil))
    	(ignore-errors (numberp (read-from-string token)))
    )
)


(defun removeItemsFromList (L item)
	(loop for i in L
		when (eql i item)
			do (remove i L)
	)
)

(defun splitBySpace (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun printem (&rest args)
  (format t "~{~a~^ ~}" args))