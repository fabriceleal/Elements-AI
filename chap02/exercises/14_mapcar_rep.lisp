

(defun repeat (lst)
	(mapcar (lambda(x)(list x x)) lst) )


(print
	(equal 
		(repeat '(x y (z w)))
		'((x x) (y y) ((z w) (z w)))
	) )
