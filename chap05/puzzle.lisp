
(load "../appendix/FUNCTIONS.lisp")

; Order: south - east - north - west
(putprop 'p1 '(st ha gr st) 'pattern)
(putprop 'p2 '(bx st ha bx) 'pattern)
(putprop 'p3 '(st ha bx gr) 'pattern)
(putprop 'p4 '(gr gr ha bx) 'pattern)

; Piece order in status
; (bottom_right bottom_left top_right top_left)

(setq pieces_avail '(p1 p2 p3 p4))

(setq box_width 2)
(setq box_length 2)


(defun orient (piece orientation)
	(rotate_list (get piece 'pattern) (- orientation 1)) )


(defun rotate_list (l n)
	(cond ((zerop n) l) 
		(t (rotate_list 
			(cons (car (last l)) (all_but_last l))
			(- n 1) )) ) )
; common-lisp's last returns the last valid cdr, which is a list.
; we want an elem, and hence the little hack above. No need to defun another last.


(defun all_but_last (l)
	(cond ((null (cdr l)) nil) 
		(t (cons (car l) (all_but_last (cdr l)))) ) )

; Robust length function.
(defun mlength (lst)
	(cond ((or (null lst) (not (listp lst))) 0)
		(t (+ 1 (length (cdr lst))))))

(defun sidesok (new_piece orientation cur_state)
	(cond ((null cur_state) t)
		(t (prog (trial len)
			(setq trial (orient new_piece orientation))
			(setq len (mlength cur_state))
			(cond 
				((zerop (mod len box_width)) (return (matchnorth trial cur_state)))
				((< len box_width) (return (matchwest trial cur_state)))
				(t (return (and (matchnorth trial cur_state) (matchwest trial cur_state))))
			) )) ) )



; Checks if the trial placement agrees with the neighbor to the north
(defun matchnorth (trial state)
	(eq (caddr trial) ; north side of rotated new piece
		(car (apply 'orient 			
			(nth (- box_width 1) state) )) ) )
				; south side of square to the north



; Checks if the trial placement agrees with the neighbor to the west
(defun matchwest (trial state)
	(eq (cadddr trial) ; west side of rotated new piece
		(cadr (apply 'orient 
			(car state) )) ) )
				; east side of square to the west



(defun solve_squares (cur_state unused_pieces)
	(cond ((null unused_pieces) (show cur_state)) ; Solution found ...
		(t 
			(mapcar 
				'trypiece 
				(mapcar (lambda(x) (list x cur_state unused_pieces)) unused_pieces))
			nil) ) )


(defun show (soln)
	(prog () 
		;(break)
		(setq mcount (+ mcount 1)) 
		(prin1 'solution)	
		(princ " ")
		(princ mcount)
		(princ ":")
		(print soln) 
		(terpri) ) )


(defun test ()
	(prog () ;(mcount)
		(setq mcount 0)
		(solve_squares nil pieces_avail) ) )


(defun trypiece (lst_piece_curstate) ;(piece)
	(let ((piece (car lst_piece_curstate)) 
		(cur_state (cadr lst_piece_curstate))
		(unused_pieces (caddr lst_piece_curstate)) )

		(mapcar 'tryorientation 
			(mapcar 
				(lambda(x) (list piece x cur_state unused_pieces)) 
				'(1 2 3 4) ) ) ) )


(defun tryorientation (lst_piece_or)
	(let ((piece (car lst_piece_or)) 
		(orientation (cadr lst_piece_or)) 
		(cur_state (caddr lst_piece_or))		
		(unused_pieces (cadddr lst_piece_or)) )

		(cond ((sidesok piece orientation cur_state)
			(solve_squares
				(cons (list piece orientation) cur_state)
				(delete piece unused_pieces) ) 
			;(princ "piece: ") (princ piece) (terpri)
			;(princ "unused: ")(princ unused_pieces)(terpri)
			;(princ "avail: ") (princ pieces_avail)(terpri)
			)
			(t nil) ) ) )


;(trace tryorientation)
;(trace trypiece)
;(trace solve_squares)
;(trace sidesok)
;(trace matchnorth)
;(trace matchwest)
