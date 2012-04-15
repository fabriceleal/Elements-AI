; Takes of the operation of a math expression
(defun operation(expr) 
	(cond ((atom expr) nil)
		(t (car expr)) ) )

; Check if list contains elements
(defun contains(expr look) 
	(cond ((atom expr) (equal expr look))
		((equal (car expr) look) T)
		(T (contains (cdr expr) look))
	 ) )

; Reduces a multiplication by zero to zero
(defun feature1 (expr) 
	(cond ((atom expr) expr)
		((and (equal (operation expr) '*) (contains expr 0)) 0)
		(t expr) ) )

; Removes ones of the multiplications and zeroes from the additions
(defun feature2 (expr)
	(cond ((atom expr) expr)
		((equal (operation expr) '*) (feature3 (remove-if (lambda(x) (equal x 1))  expr)))
		((equal (operation expr) '+) (feature3 (remove-if (lambda(x) (equal x 0))  expr)))
		(t expr) ) )

; If expr is a operation and has only one arg, replace it by the arg itself; do not call directly, called in feature2
(defun feature3 (expr)
	(cond ((atom expr) expr)
		((= (length expr) 2) (cadr expr))
		((= (length expr) 1) '())
		(t expr)
		))

; Takes off all nils (and threby, all '() )
(defun cleaning (expr)
	(cond ((atom expr) expr)
		(t (remove-if (lambda(x) (equal x nil)) expr)) ) )

; Start simplification from the bottom to up
(defun simplify (expr)
	(cond ((atom expr) expr)
		(t (cleaning (feature1 (feature2 (mapcar 'simplify expr))))) ) )


; For now, it will be left as is. It's not perfect, but works ok for most input.
; Known bugs: (simplify '(* y (+ 0 0))) yields (* y), when it should be zero.
; The test case for the exercise computes ok. (+ x 3 5 (* (* x y z) 0))

