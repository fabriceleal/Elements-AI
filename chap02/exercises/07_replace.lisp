
; Replacing all s2 by s3 wherever it occurs in s1
(defun myreplace(s1 s2 s3)
	(cond ((equal s1 s2) s3) ; Whenever s1 equals s2, return s3
		((atom s1) s1) ; If s1 is an atom, return itself
		(t (cons (myreplace (car s1) s2  s3) (myreplace (cdr s1) s2  s3))) ; otherwise, cons the result of computing car with the result of computing cdr
		) )

; Test, should print T
(print
	(equal
		(myreplace '((this 1) contains (2 occurences (this 1))) '(this 1) '(that one))
		'((that one) contains (2 occurences (that one))) ) )
