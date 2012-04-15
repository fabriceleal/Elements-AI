
; Prog example at work
(prog 
	(x y) ; List of variables
	(setq x 10) ; Statements ...
	(setq y 0)
lblloop (print (* y y)) ; Loop labeled with lblloop ...
	(setq x (- x 1))
	(setq y (+ y 1))
	(cond ((zerop x) (return y)) ; 'return' gets out of prog with a return value
		(t (go lblloop)) ; 'go' jumps to a label
	)
)
