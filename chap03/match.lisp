
; The dummy beginning of the all-mighty match!
(defun match1 (p s) (equal p s))

(defun match2 (p s)
	(cond 
		((atom p) (atom s)) ; Returns true if both are atoms
		((atom s) nil) ; If p is not an atom but s is, returns null
		((match2 (car p) (car s))
		 (match2 (cdr p) (cdr s)) ) ; If the heads match, return true if the tails also do, otherwise return null
		(t nil) ) ) ; If everything fails, return null

; Now:
; (match '(1 2 3) '(a b c)) yields T
; (match '(1 (2) 3) '(a (b) c)) yields T
; (match '(1 (2) 3) '(a b c)) yields NIL

(defun match3 (p s)
	(cond 
		((null p) (null s)) ; if both are null, match!
		((or (atom p) (atom s)) nil) ; if p is not null and either p or s is an atom, return nil
		((equal (car p) (car s)) 
		 (match3 (cdr p) (cdr s)) ) ; If the cars equal, try match the rest.
		((eq (car p) '?) (match3 (cdr p) (cdr s))) ; joker clause. Match anything with a ?
		(t nil)  ; extremal clause
		
		) )

; Now:
; (match3 '(? a b c) '(1 a b c)) yields T
; (match3 '(? a b c) '(1 2 3 4)) yields NIL
; (match3 '(?  ca b) '((1 2 3) a b c)) yields T

(defun match4 (p s)
        (cond
                ((null p) (null s)) ; if both are null, match!
                ((or (atom p) (atom s)) nil) ; if p is not null and either p or s is an atom, return nil
                ((equal (car p) (car s))
                 (match4 (cdr p) (cdr s)) ) ; If the cars equal, try match the rest.
                ;((eq (car p) '?) (match4 (cdr p) (cdr s))) ; joker clause. Match anything with a ?
                ((and
			(equal (length (car p)) 2) ; The head of p should be a sublist of length 2
			(eq (caar p) '?) ; The head of the head of p should be qual to an ?
			(match4 (cdr p) (cdr s)) ) ; The tail of p should match the tail of s
		 (set (cadar p) (car s)) ; Set the head of the tail of the head of p(the second element of (car p) ) to the value of the head of s
		 t ) ; ... and return true
		(t nil)  ; extremal clause
                ) )


; Now:
; (match4 '((? x) 2 3 4) '(a 2 3 4)) => T and x = a
; (match4 '((? x) 2 3 4) '((1 2 3) 2 3 4)) => T and x = (1 2 3)

(defun match5 (p s)
        (cond
                ((null p) (null s)) ; if both are null, match!
                ((or (atom p) (atom s)) nil) ; if p is not null and either p or s is an atom, return nil
                ((equal (car p) (car s))
                 (match5 (cdr p) (cdr s)) ) ; If the cars equal, try match the rest.
		; MATCH ANYTHING WITH A ?, SET TO THE VAR
		((and
                        (equal (length (car p)) 2) ; The head of p should be a sublist of length 2
                        (eq (caar p) '?) ; The head of the head of p should be qual to an ?
                        (match5 (cdr p) (cdr s)) ) ; The tail of p should match the tail of s
		 (set (cadar p) (car s)) ; Set the head of the tail of the head of p(the second element of (car p) ) to the value of the head of s
                 t ) ; ... and return true
		; MATCH WITH A PREDICATE, SET TO THE VAR ON SUCCESS
                ((and
                        (equal (length (car p)) 2) ; The head of p should be a sublist of length 2
			(null (eq (caar p) '?)) ; Tries to do a "any" matching
			(apply (caar p) (list (car s))) ; Applies the predicate supplied to the element to match
                        (match5 (cdr p) (cdr s)) ) ; The tail of p should match the tail of s
                 (set (cadar p) (car s)) ; Set the head of the tail of the head of p(the second element of (car p) ) to the value of the head of s
                 t ) ; ... and return true

                (t nil)  ; extremal clause
                ) )

; Now:
; (match5 `((,(lambda(x) (< x 555)) x) 2 3 4) '(1111 2 3 4)) => NIL
; (match5 `((,(lambda(x) (< x 555)) x) 2 3 4) '(554 2 3 4)) => T, and x = 554
; (match5 '((numberp x) 2 3 4) '(1111 2 3 4)) => T, and x = 1111
; (match5 '((?  2x) 3 4) '(1 2 3 4)) => T, and x = 1

(defun match6 (p s)
        (cond
                ((null p) (null s)) ; if both are null, match!
; From here, p is not null
                ((atom (car p)) ; head of p must be an atom
		 (and s ; s must be not null
		 	(equal (car p) (car s)) ; the heads of p and s must match, and if they do ...
                 	(match6 (cdr p) (cdr s)) ) ) ; ... try match the rest.
; From here, the car of p is non-atomic
                ; MATCH ANYTHING WITH A ?, SET TO THE VAR 
                ((and
			s ; s is not null
                        (eq (caar p) '?) ) ; The head of the head of p should be qual to an ?

                 (cond ((match6 (cdr p) (cdr s))
			(set (cadar p) (car s))
			t)
			(t nil)) )	
		; MATCH WITH *
		((eq (caar p) '*)
		 (cond 
			((and s (match6 (cdr p) (cdr s))) 	; subcase 1
			 (set (cadar p) (list (car s))) t )
			((match6 (cdr p) s) 			; subcase2
			 (set (cadar p) nil) t )
			((and s (match6 p (cdr s)))		; subcase 3
			 (set (cadar p) (cons (car s) (eval (cadar p)))) t )
			(t nil) ) )

		; MATCH WITH A PREDICATE, SET TO THE VAR ON SUCCESS
                ((and
			s ; s not null
                        (apply (caar p) (list (car s))) ; Applies the predicate supplied to the element to match
                        (match6 (cdr p) (cdr s)) ) ; The tail of p should match the tail of s
                 (set (cadar p) (car s)) ; Set the head of the tail of the head of p(the second element of (car p) ) to the value of the head of s
                 t ) ; ... and return true
                (t nil)  ; extremal clause
                ) )

; (match6 '((* X) wild (? y) (* z)) '(* specifies a wild card sequence element))
; => T, X = (* specifies a), Y = card e Z = (sequence element)

(defun match(p s) (match6 p s))
