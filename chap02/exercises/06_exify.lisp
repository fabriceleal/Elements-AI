

; Replaces all non-nil entries of s1 by x
(defun exify (s1)
	(cond ((null s1) nil) ; s1 is null
		((atom s1) 'x) ; s1 is a non-null atom
		(t (cons (exify (car s1)) (exify (cdr s1)))) ; s1 is a list or a dotted list
	) )


; Test, should write T
(print 
	(equal
		(exify '(a (b . c) x y nil z))
		'(x (x . x) x x nil x) ) )

