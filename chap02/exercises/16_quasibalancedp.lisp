(load "exercises/15_equalelts.lisp")

(defun length-list (lst)
	(cond
              ((listp lst) (length lst))
              (t -1) ) )

; Returns a list with the length of the sublists
(defun length-sublists (lst)
	(cond ((atom lst) (list -1))
		(t (mapcar 'length-list lst)) ) )

; Returns a list with the max depth of the list
(defun depth-list (lst)
	(cond ;((atom lst) 0)
		((listp lst) (+ 1 (apply 'max (mapcar 'depth-list lst))))
		(t 0) ) )

; Returns a list with the max depth of the sublists
(defun depth-sublists (lst)
	(cond ((atom lst) (list -1))
		(t (mapcar 'depth-list lst)) ) )

; Returns T if:
; * all top level sublists have the same length
; * all top level sublists have the same depth
; * each sublist is also quasi_balanced
(defun quasi_balancedp (lst)
	(cond 
		((atom lst) T)
		((and (equalelts (length-sublists lst)) (equalelts (depth-sublists lst)) (equalelts (cons t (mapcar 'quasi_balancedp lst)))) t)
		(t nil) ) )


; Test, should return T, NIL, NIL
(print (quasi_balancedp '(((1 1) (2 2)) ((3 3) (4 4)) ((5 5) (6 6)))))
(print (quasi_balancedp '(((1) (2 2)) ((3 3) (4)) ((5 5) (6 6)))))
(print (quasi_balancedp '(((1 1) (2 2)) ((3 3) (4 4)) ((5 5) ))))
