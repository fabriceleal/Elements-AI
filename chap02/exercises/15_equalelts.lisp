
; returns true only if all the top level elements of lst are equal to each other
(defun equalelts (lst)
	(cond ((or (atom lst) (= (length lst) 1)) T) 
		; lst is an atom or an one elem list
		
		((and (equal (car lst) (cadr lst)) (equalelts (cdr lst))) T) 
		; arbitrary length list: equal if the cdr are all equals, and if the car equals the cadr of the list
		
		(t nil)
	) )
