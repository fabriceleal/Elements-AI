

(defun len(lst)
	"Recursive definition of length"
	(cond ((null lst) 0) ; Base case
		(t (+ 1 (len (cdr lst)))) ; Recursive
	))

(defun count-sublists(lst)
	"Counts nested sublists in a list"
	(cond ((null lst) 0) ((atom lst) 0) ; Base case 1,2
		((atom (car lst)) (count-sublists (cdr lst))) ; Recursive 1/2
		(t (+ 1 (count-sublists (car lst)) (count-sublists (cdr lst)))) ; Recursive 2/2
	))


(defun matchkth (elem lst k)
	"Match kth element of a list lst with the element elem." 
	(cond ((null lst) nil) ((< k 1) nil) ; Base case 1,2/3
		((and (= k 1) (= elem (car lst)))) ; Base case 3/3
		(t (matchkth elem (cdr lst) (- k 1))) ; Recusrive
	))

(defun rappend (lst1 lst2)
	"Recursive definition of append."
	(cond ((null lst1) lst2) ; Base case
		(t (cons (car lst1) (rappend (cdr lst1) lst2))) ; Recursive
	))
