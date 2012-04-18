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
                ((and
			s
                        (apply (caar p) (list (car s)))
                        (match7 (cdr p) (cdr s)) )
                 (set (cadar p) (car s)) ; Set the head of the tail of the head of p(the second element of (car p) ) to the value of the head of s
                 t ) ; ... and return true
                (t nil)  ; extremal clause
                ) )


