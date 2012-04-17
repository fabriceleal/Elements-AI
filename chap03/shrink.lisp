(load "match.lisp")

; The shrink program.
; The shrink has a very short "memory" - it responds with knowledge of only one input.

; Helper functions, prints a list without parentheses

; Friendly printer of a list
(defun printl (message)
	(prog ()
		(mapcar
		 	(function (lambda (txt)
				(cond ((not (null txt)) (princ txt) (princ " "))) ) )
			message ) 
		(terpri) ) )

(defun wword ()
	(prog ()
		(incf wwordcount)
		(cond ((equal wwordcount 3) (setq wwordcount 0))
			(return (nth wwordcount '(when why where)))
			) ) )

(defun wpred (w) (member w '(why where when what)))

(defun dpred (w) (member w '(do can should would)))

; Getnth doesn't need to be implemented, there is nth.

; Last resort phrases
(setq punts
	'(
	(please go on)
	(tell me more)
	(i see)
	(what does that indicate)
	(but why be concerned about it)
	(just tell me how you fell)
	) )

; Converts word from 1st to 2nd person, and vice-versa
(defun youme (w)
	(cond	((eq w 'i) 'you)
		((eq w 'me) 'you)
		((eq w 'you) 'me)
		((eq w 'my) 'your)
		((eq w 'your) 'my)
		((eq w 'yours) 'mine)
		((eq w 'mine) 'yours)
		((eq w 'am) 'are)
		(t w)
		) )

(defun youmemap (lst)
	(mapcar 'youme lst))

(defun verbp (w)
	(member w '(
		go have be try eat take help make get jump
		write type fill put turn compute think drink
		blink crash crunch add
		) ) )

; The shrink prog
(defun shrink()
	(prog ()
		(setq wwordcount 0)
		(setq puntcount 0)
		(printl '(welcome to my sofa))
		(printl '(please enclose your input in parentheses))
	my_loop (setq s (youmemap (read)))
		(cond
			((match '(bye) s)
			 (return 'goodbye))
			((match '(you are (* x)) s)
			 (printl (append '(please tell me) (list (wword)) '(you are) x)) )
			((match '(you have (* x)) s )
			 (printl (append '(how long have you had) x)) )
			((match '(you fell (* x)) s)
			 (printl '(i sometimes feel the same way)) )
			((match '(because (* x)) s)
			 (printl '(is that really the reason)) )
			((match nil s)
			 (printl '(please say something)))
			((match '(yes (* x)) s)
			 (printl (append '(how can you be so sure) x)) )
			((match '(me are (* x)) s)
			 (printl (append '(oh yeah i am) (x))) )
			((match '((verbp v) (* x)) s)
			 (printl (append `(oy yoi yoi he wants that i should go and ,v) x)) )
			((match '((wpred w) (* x)) s)
			 (printl `(you tell me ,w)) )
			((match '((dpred w) me (* x)) s)
			 (printl (append `(perhaps i ,w) x)) )
			((match '(do me think (* x)) s)
			 (printl '(i think you should answer that yourself)) )
			((member 'dream s) 
			 (printl '(for dream analysis see freud)) )
			((member 'love s) 
			 (printl '(all is fair in love and war)))
			((member 'no s) 
			 (printl '(dont be so negative)))
			((member 'maybe s) 
			 (printl '(be more decisive)))
			((member 'you s) 
			 (printl s))
			; The "can't fail" rule 
			(t 
				(incf puntcount)
				(cond ((equal puntcount 7) 
					(setq puntcount 0) ) ) 
				(printl (list (nth puntcount punts)))
			) ) 
	(go my_loop)
			) )
