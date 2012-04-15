

; Quick and dirty fact
(defun fact(x) (cond ((zerop x) 1) (t (* x (fact (- x 1))))))

(fact 3) ; Computes 6
(trace fact) ; Starts tracing our fact function
(fact 3) ; Computes 6, outputs tracing info
(untrace fact) ; Stops tracing our fact function


; Example of using break inside a function.
(defun breakable(&rest all)
	(prog () ;(p)
		;(setf p all)
		(break "Oh dear ...")
		(return 'true)
	))
