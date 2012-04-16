
; Unordered production system to convert to Roman numerals

(defun roman1 ()
	(prog (x)
		my_loop
		(cond
			((null x) (print '(Enter number)) (setq x (read)))
			((and (not (null x)) (> x 39)) (print '(Too big) (setq x nil)))
			((and (not (null x)) (< x 40) (> x 9)) (princ 'X) (decf x 10))
			((and (not (null x)) (= x 9)) (princ 'IX) (decf x 9))
			((and (not (null x)) (< x 9) (> x 4)) (princ 'V) (decf x 5))
			((and (not (null x)) (= x 4)) (princ 'IV) (setq x 0))
			((and (not (null x)) (< x 4) (> x 0)) (princ 'I) (decf x))
			( (zerop x) (setq x nil) (return nil))
			
		) 
	(go my_loop) ) )



; Ordered production system. Notice that less tests are made on the parameter x.

(defun roman2 ()
	(prog ()
		my_loop
		(cond 
			((null x) (print '(Enter number)) (setq x (read)))
			((> x 39) (print '(Too big) (setq x nil)))
			((> x 9) (princ 'X) (decf x 10))
			((= x 9) (princ 'IX) (decf x 9))
			((> x 4) (princ 'V) (decf x 5))
			((= x 4) (princ 'IV) (decf x 4))
			((> x 0) (princ 'I) (decf x))
			((zerop x) (setq x nil) (return nil))
	) 
	(go my_loop)) )


; Implemented as a descrimination net.
; A descrimination net reduces the maximum number of subcondition tests,
; but it will make more dificult to implement new features (less simplicity and less modularity)


(defun roman3 ()
	(prog ()
		my_loop
		(cond 
			((null x) (print '(Enter number)) (setq x (read)))
			(t (cond 
				((> x 39) (print '(Too big)) (setq x nil))
				(t (cond
					((> x 4) (cond
							((> x 9) (princ 'X) (decf x 10))
							(t (cond 
								((= x 9) (princ 'IX) (decf x 9))
								(t (princ 'V) (decf x 5))
							) )
					) )
					(t (cond 
						((= x 4) (princ 'IV) (decf x 4))
						(t (cond
							((> x 0) (princ 'I) (decf x))
							(t (setq x nil) (return nil))
						) )
					) )
				) )
			) )
	)	
	(go my_loop) ) )


