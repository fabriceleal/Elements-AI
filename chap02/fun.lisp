

(defun mult(&rest all)
	"Receives an arbitrary number of parameters, all put in a list."
	(mapcar(lambda(x)(* x x)) all))

(mult 1 2 3 4 5) ; Should return (1 4 9 16 25)
