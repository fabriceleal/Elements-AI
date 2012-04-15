
(defun my-reverse (lst)
	(cond ((or (atom lst) (null lst)) lst) ; No list
		((null (cdr lst)) lst) ; List with one element, return the list itself
		(t (append (my-reverse (cdr lst)) (cons (car lst) nil))) ; List with n elements, append the result of reversing the cdr with the car
		) )

(defun palindrome (lst)
	(cond ((or (null lst) (atom lst)) nil)
		(t (equal lst (my-reverse lst)))
		) )
