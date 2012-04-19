(load "match.lisp") ; The one at the chap03/ dir :)

; Aux
(defun increasing (lst)
	(cond ((or (null lst) (atom lst) (= 1 (length lst))) nil) ; Return nil if it is an atom / null
		; The list of length 1 returns true if the member is integer
		((and (= 2 (length lst)))
			(and (numberp (car lst)) (numberp (cadr lst)) (< (car lst) (cadr lst))))
		; For lists greather than zero, return true if the first
		; is less than the second, and check recursively
		((and (numberp (car lst)) (numberp (cadr lst)) (< (car lst) (cadr lst))) 
			(increasing (cdr lst)) )
		(t nil)
		) )

(defun match7 (p s)
        (cond
                ((null p) (null s)) ; if both are null, match!
; From here, p is not null
                ((atom (car p)) ; head of p must be an atom
		 (and s ; s must be not null
		 	(equal (car p) (car s)) ; the heads of p and s must match, and if they do ...
                 	(match7 (cdr p) (cdr s)) ) ) ; ... try match the rest.
; From here, the car of p is non-atomic
                ; MATCH ANYTHING WITH A ?, SET TO THE VAR 
                ((and
			s ; s is not null
                        (eq (caar p) '?) ) ; The head of the head of p should be qual to an ?

                 (cond ((match7 (cdr p) (cdr s))
			(set (cadar p) (car s))
			t)
			(t nil)) )	
		; MATCH WITH *
		((eq (caar p) '*)
		 (cond 
			((and s (match7 (cdr p) (cdr s))) 	; subcase 1
			 (set (cadar p) (list (car s))) t )
			((match7 (cdr p) s) 			; subcase2
			 (set (cadar p) nil) t )
			((and s (match7 p (cdr s)))		; subcase 3
			 (set (cadar p) (cons (car s) (eval (cadar p)))) t )
			(t nil) ) )

		; MATCH WITH A PREDICATE, SET TO THE VAR ON SUCCESS
		; these functions match lists of length 2, so we
		; must take into account that s must be of length >= 2

		; Base case: the list has 2 elements, try to match on those 2
		((and s (= (length s) 2))
		 (cond (
			(apply (caar p) (list (list (car s) (cadr s))))

			(set (cadar p) (list (car s) (cadr s)))
			t
			)
			(t nil)	
		))
		
		; Complex case: the list has more than 2 elements
		((and s (> (length s) 2))
		(cond
			; Case 1
			; The 2 first elements match and the pattern
			; matches the rest of the s-expression
			; set var as a list consisting of the first element
			; of the list and the cdr is whatever already was there
			((and 					
				(apply (caar p) (list (list (car s) (cadr s))))
				(match7 p (cdr s)) )
			
				(set (cadar p) (cons (car s) (eval (cadar p))))
				t )		

                        ; Case 2
                        ; The 2 first elements match and the rest of the
                        ; pattern matches the rest of the s-expression
                        ; set var to a list of 2 elements
                        ((and
                                (apply (caar p) (list (list (car s) (cadr s))))
                                (match7 (cdr p) (cdr (cdr s))) )
                                        
                                (set (cadar p) (list (car s) (cadr s)))
                                t )


			) )


                (t nil)  ; extremal clause
                ) )

; These works:
; (match7 '(101 (increasing x) (increasing y)) '(101 2 4 6 8 3 5 7))
; x = (2 4 6 8)
; y = (3 5 7)

; (match7 '(101 (increasing x) (* y)) '(101 2 4 6 8 3 5 7))
; x = (2 4 6 8)
; y = (3 5 7)

; (match7 '(101 (increasing x) a) '(101 2 4 6 8 a))
; x = (2 4 6 8)
