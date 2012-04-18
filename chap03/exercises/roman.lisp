(defun roman2 ()
	(prog (x)
		my_loop
		(cond 
			((null x) (print '(Enter number)) (setq x (read)))
; Changed on exercise 2
			((> x 399) (print '(Too big) (setq x nil)))
			((> x 99) (princ 'C) (decf x 100))
			((> x 49) (princ 'L) (decf x 50))
; End exercise 2
			((> x 9) (princ 'X) (decf x 10))
			((= x 9) (princ 'IX) (decf x 9))
			((> x 4) (princ 'V) (decf x 5))
			((= x 4) (princ 'IV) (decf x 4))
			((> x 0) (princ 'I) (decf x))
			((zerop x) (setq x nil) (return nil))
	) 
	(go my_loop)) )
