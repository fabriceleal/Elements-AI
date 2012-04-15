

(defun createclojure ()
	(let ((y 5))
		(lambda(x) (+ y x))))


(apply (createclojure) 1) ; Should print 6, regardless of the value of y in the 'global' scope

