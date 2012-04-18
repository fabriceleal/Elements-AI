
(defun german_map_name (nbr) ; 1 .. 9
	(nth (- nbr 1) '(eins zwei drei vier fünf sechs sieben acht neun)))



(defun german_nos ()
	(prog (x) 
	my_loop
		(cond ((null x) (print '(Enter number)) (setq x (read))) 
			((> x 99) (print '(Number is too big)) (setq x nil))
			; Production rules here

			; Returns
			((zerop x) (return nil))

			; Prints irregulars (10, ...)
			((= x 10)  (princ 'zehn) (terpri) (setq x 0))
			((= x 11)  (princ 'elf) (terpri) (setq x 0))
			((= x 12)  (princ 'zwölf) (terpri) (setq x 0))
			((= x 16)  (princ 'sechzehn) (terpri) (setq x 0))
			((= x 17)  (princ 'siebzehn) (terpri) (setq x 0))
			((= x 20)  (princ 'zwanzig) (terpri) (setq x 0))

			; Prints exactly 20, 30, 40, 50 ...
			((zerop (mod x 10)) (princ (german_map_name (floor (/ x 10))))(princ 'zig) (setq x 0))
		
			; Prints 1*, 2*, 3*, 4*, 5* ... first prints the number * then prints 10, 20, 30, 40, 50 then ...
			((> x 10)
				(let* ((u (mod x 10)) (d (floor (/ x 10))))
					; When u = 1 prints ein instead of eins
					(princ (if (> u 1) (german_map_name (mod x 10)) 'ein))
					; When d = 1 dont print the "und"
					(princ (if (> d 1) 'und ""))
					(setq x (- x (mod x 10))) )
					)

			; Prints 1 to 9
			((> x 0)  (princ (german_map_name x)) (terpri) (setq x 0)) )

		(go my_loop) ) )
